
#eBird filtering and detection probability (encounter rate) modeling
#referenced from Strimas-Mackey et al. Best Practices for Using eBird Data.

#Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, O.J. Robinson, E.T. Miller,
#T. Auer, S. Kelling, D. Fink, A. Johnston. 2020. Best Practices for Using eBird Data.
#Version 1.0. https://cornelllabofornithology.github.io/ebird-best-practices/. 
#Cornell Lab of Ornithology, Ithaca, New York. https://doi.org/10.5281/zenodo.3620739

library(lubridate)
library(sf)
library(tidyverse)
library(ranger)
library(scam)
library(PresenceAbsence)


#-----------------
#DATA PREPARATION
#-----------------
memory.limit(100000)

load("Objects and Data/2_filtered_ebird_data.rda")
load("Objects and Data/0_MA_town_shapefiles.rda")
ma_towns<-ma_towns[!duplicated(ma_towns$city),]
load("Objects and Data/2_habitat_elev_year_MAtowns.rda")
load("Objects and Data/0_species_list.rda")

RF_town_assess<-list()
RF_state_assess<-list()
ebirdRF_results_town<-list()
ebirdRF_results_town_sf<-list()
ebirdRF_results_state<-list()
t_peak<-list()
peak_dow<-list()

#prediction surface (MA towns each week 2005-2018)
pred_town_eff <- dplyr::select(ma_towns,city) %>% 
  st_drop_geometry()%>%
  dplyr::mutate(year = rep(c(2005:2018), length.out = length(city)),
                week = rep(unique(ebird_ss_balanced[[1]]$week),length.out=length(city)))%>%
  complete(year, city, week)%>%
  dplyr::mutate(time_observations_started = NA,
                day_of_week = NA,
                protocol_type="Traveling",
                LLA=NA)%>%
  left_join(dplyr::select(pland_elev_towns,-c("TOWN_ID","Area_km2")),by=c("city","year"))%>%
  rename(Year=year)%>%
  left_join(ma_towns,by="city")
pred_town_eff<-pred_town_eff[!duplicated(pred_town_eff), ]


#---------------------------------------
#EBIRD DETECTION PROBABILITY MODELING
#---------------------------------------

set.seed(1)
for (j in 1:length(name_list$sp_file)) {
  
  #for each bird species...
  ebird_ss<-ebird_ss_balanced[[j]]

  # combine ebird and habitat data
  ebird_habitat <- inner_join(ebird_ss, pland_elev_towns, by = c("city", "year"))
  
  #1. RANDOM FOREST PREDICTIONS
  #------------------------------
  
  #split data into testing and training data
  ebird_split <- ebird_habitat %>% 
    # select only the columns (variables) to be used in the model
    dplyr::select(species_observed,
           Year, day_of_week,week,
           time_observations_started, 
           protocol_type,
           LLA,
           city,
           Barren_Land,Cultivated_Crops,Developed,Forest,
           Grassland,Open_Water,Shrub_Scrub,Wetland,
           elevation_median,elevation_sd) %>% 
    drop_na()
  
  set.seed(1)
  # split 80/20
  ebird_split <- ebird_split %>% 
    split(if_else(runif(nrow(.)) <= 0.8, "train", "test"))
  
  #Since we have class imbalance (greater non detections than detections) we will use a balanced random forests approach.
  #It samples an equal proportion of detections and non detections in each random sample.
  
  #To use this approach we need to calculate the proportion of detections in the dataset:
  detection_freq <- mean(as.logical(ebird_split$train$species_observed))
  
  
  # ranger requires a factor response to do classification
  ebird_split$train$species_observed <- factor(ebird_split$train$species_observed)
  # grow random forest, sample.fraction parameter addresses the balanced sampling, tells model to sample equal detections/nondetections
  #probabilies=TRUE returns the probabilities rather than just returning the most probable class (1 or 0)
  rf <- ranger(formula =  species_observed ~ ., 
               data = ebird_split$train,
               importance = "impurity",
               probability = TRUE,
               replace = TRUE, 
               sample.fraction = c(detection_freq, detection_freq))
  
  #Probabilities don't always align with detection frequency, especially since we resampled and increased the detection ratio.
  #Producing a calibration of the predictions can help better diagnose the predictions and realign the predictions with observations.
  #To view calibrations, predict encounter rate for each checklist in training data and then fit binomial GAM with real observed encounter
  #rate as the response and predicted encounter rate as the predictor variable. 
  #Use GAM constrained to only increase since we apriori expect encounter rate to increase with predicted encounter rate. Use scam() to do this.
  
  # make predictions on training data (we want probabilities of detection which is the second column in predictions)
  occ_pred <- rf$predictions[, 2]
  # convert the observed response back to a numeric value from factor
  occ_obs <- ebird_split$train$species_observed %>% 
    as.logical() %>% 
    as.integer()
  rf_pred_train <- tibble(obs = occ_obs, pred = occ_pred) %>% 
    drop_na()
  
  # fit calibration model
  calibration_model <- scam(obs ~ s(pred, k = 5, bs = "mpi"), 
                            gamma = 1.4,
                            data = rf_pred_train)
  
  # calculate the average observed encounter rates for different categories of estimated encounter rates 
  
  average_encounter <- rf_pred_train %>%
    mutate(pred_cat = cut(rf_pred_train$pred, breaks = seq(0, 1, by=0.02))) %>%
    group_by(pred_cat) %>%
    dplyr::summarise(pred = mean(pred), obs = mean(obs), checklist_count = n()) %>%
    ungroup()
  
  cal_pred <- tibble(pred = seq(0, 1, length.out = 100))
  cal_pred <- predict(calibration_model, cal_pred, type = "response") %>% 
    bind_cols(cal_pred, calibrated = .)
  
  #model shows estimated encounter rates are fairly balanced with observed encounter rates. sites with higher estimations have higher
  #observed encounters. Model is good at estimating accurately and can distinguish sites with low rates from high rates.
  
  #If accurate encounter rates are required, calibration model can be used to calibrate the estimates from the RF model, so they 
  #are adjusted to match the observations more closely. Calibrated RF model is combination of the original RF followed by the calibration model.
  
  
  
  #2. MODEL ASSESSMENT
  #---------------------
  #Assess the quality of both uncalibrated and calibrated models in predicting observed patterns of occupancy using validation data from the 20% test data set.
  #Predictive performance metrics to compare the predictions to the actual observations (require binary thresholds for probabilites): 
  #mean squared error (MSE), sensitivity, specificity, AUC, and Kappa.
  
  # predict on test data using calibrated model
  p_fitted <- predict(rf, data = ebird_split$test, type = "response")
  # extract probability of detection
  p_fitted <- p_fitted$predictions[, 2]
  # calibrate
  p_calibrated <- predict(calibration_model, 
                          newdata = tibble(pred = p_fitted), 
                          type = "response")
  rf_pred_test <- data.frame(id = seq_along(p_calibrated),
                             # actual detection/non-detection
                             obs = as.logical(ebird_split$test$species_observed),
                             # uncalibrated prediction
                             fit = p_fitted,
                             # calibrated prediction
                             cal = p_calibrated) %>%
    # constrain probabilities to 0-1
    mutate(cal = pmin(pmax(cal, 0), 1)) %>% 
    drop_na()
  
  # mean squared error (mse)
  mse_fit <- mean((rf_pred_test$obs - rf_pred_test$fit)^2, na.rm = TRUE)
  mse_cal <- mean((rf_pred_test$obs - rf_pred_test$cal)^2, na.rm = TRUE)
  
  # pick threshold to maximize kappa
  opt_thresh <- optimal.thresholds(rf_pred_test, opt.methods = "MaxKappa")
  
  # calculate accuracy metrics: auc, kappa, sensitivity, specificity,
  metrics_fit <- rf_pred_test %>% 
    dplyr::select(id, obs, fit) %>% 
    presence.absence.accuracy(threshold = opt_thresh$fit, 
                              na.rm = TRUE, 
                              st.dev = FALSE)
  metrics_cal <- rf_pred_test %>% 
    dplyr::select(id, obs, cal) %>% 
    presence.absence.accuracy(threshold = opt_thresh$cal, 
                              na.rm = TRUE, 
                              st.dev = FALSE)
  
  rf_assessment <- tibble(
    model = c("RF", "Calibrated RF"),
    mse = c(mse_fit, mse_cal),
    sensitivity = c(metrics_fit$sensitivity, metrics_cal$sensitivity),
    specificity = c(metrics_fit$specificity, metrics_cal$specificity),
    auc = c(metrics_fit$AUC, metrics_cal$AUC),
    kappa = c(metrics_fit$Kappa, metrics_cal$Kappa)
  )
 
  
  
  RF_town_assess[[j]]<-rf_assessment
  
  
  #3. PREDICTOR IMPORTANCE
  #---------------------------
  pi <- enframe(rf$variable.importance, "predictor", "importance")
  
  # top predictors 
  top_pred <- pi %>% 
    top_n(n = 8, wt = importance) %>% 
    arrange(desc(importance))

  top_pred
  #LLA (List Length), time of day are important predictors

  #Partial dependence function. 
  #Takes the following arguments:
  # 1. predictor: name of predictor to calculate partial dependence for
  # 2. model: the encounter rate model
  # 3. data: the original data used to train the model
  # 4. x_res: the resolution of the grid over which to calculate partial dependence
  # 5. n: number of points to subsample from the training data
  
  # function to calculate partial dependence for a single predictor
  calculate_pd <- function(predictor, model, data, 
                           x_res = 25, n = 1000) {
    # create prediction grid
    rng <- range(data[[predictor]], na.rm = TRUE)
    x_grid <- seq(rng[1], rng[2], length.out = x_res)
    grid <- data.frame(covariate = predictor, x = x_grid, 
                       stringsAsFactors = FALSE)
    names(grid) <- c("covariate", predictor)
    
    set.seed(1)
    # subsample training data
    n <- min(n, nrow(data))
    s <- sample(seq.int(nrow(data)), size = n, replace = FALSE)
    data <- data[s, ]
    
    # drop focal predictor from data
    data <- data[names(data) != predictor]
    grid <- merge(grid, data, all = TRUE)
    
    # predict
    p <- predict(model, data = grid)
    
    # summarize
    pd <- grid[, c("covariate", predictor)]
    names(pd) <- c("covariate", "x")
    pd$pred <- p$predictions[, 2]
    pd <- dplyr::group_by(pd, covariate, x) %>% 
      dplyr::summarise(pred = mean(pred, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    return(pd)
  }
  #Now we'll use this function to calculate partial dependence for the top predictors.
  
  # calculate partial dependence for each predictor
  # map is used to iteratively apply calculate_pd to each predictor
  pd <- top_pred %>% 
    dplyr::mutate(pd = purrr::map(predictor, calculate_pd, model = rf, 
                                  data = ebird_split$train),
                  pd = purrr::map(pd, ~ .[, c(2, 3)]),
                  pd = purrr::map(pd, set_names, nm = c("value",  "encounter_rate"))) %>% 
    unnest(cols = pd)
  
  # calibrate predictions
  pd$encounter_rate <- predict(calibration_model, 
                               newdata = tibble(pred = pd$encounter_rate), 
                               type = "response") %>% 
    as.numeric()
  
  # Rates increase steadily with year which may reflect the increase in data availability over the years.
  # Survey distance and number of observers decreases the overall encounter rate.
  
  
  #4. CREATE SPATIAL PREDICTION SURFACE (use towns as spatial predictors)
  #---------------------------------------------------------------------
  #For prediction data, use peak time of day, peak day of each month, and a standard checklist traveling 1km for 1hr. 
  #Make predictions for all months of all years (2005-2018).
  
  # find peak time of day from partial dependence
  pd_time <- calculate_pd("time_observations_started",
                          model = rf, 
                          data = ebird_split$train,
                          # make estimates at 30 minute intervals
                          # using a subset of the training dataset
                          x_res = 2 * 24, n = 1000) %>% 
    transmute(time_observations_started = x, encounter_rate = pred)
  
  # hours with at least 1% of checklists
  search_hours <- ebird_split$train %>% 
    mutate(hour = floor(time_observations_started)) %>%
    group_by(hour)%>%
    dplyr::summarise(n=n())%>% 
    ungroup()%>%
    mutate(pct = n / sum(n)) %>% 
    filter(pct >= 0.01)
  
  # constrained peak time
  t_peak[[j]] <- pd_time %>% 
    filter(floor(time_observations_started) %in% search_hours$hour) %>% 
    top_n(1, wt = desc(time_observations_started)) %>% 
    pull(time_observations_started)
  
  # Find peak day of week using occurrence frequency
   
  occur_w <- function(data) {data %>% 
      dplyr::group_by(day_of_week) %>%
      dplyr::summarise(n_checklists = n(),
                       n_detected = sum(as.integer(species_observed)),
                       det_freq = mean(as.integer(species_observed)))
  }
  
  peak_w <- occur_w(ebird_split$train) %>%
    dplyr::group_by(day_of_week) %>%
    dplyr::filter(det_freq == max(det_freq))
  peak_w <- peak_w[!duplicated(peak_w$day_of_week),]
  peak_dow[[j]]<-peak_w$day_of_week[which(peak_w$det_freq == max(peak_w$det_freq))]
  
  # add species specific covariates to town prediction surface
  pred_town_eff2<-pred_town_eff%>%
    mutate(time_observations_started = t_peak[[j]],
           day_of_week = peak_dow[[j]],
           LLA=mean(ebird_ss[["LLA"]],na.rm=T),
           date=lubridate::parse_date_time(paste(Year, week, 1, sep="/"),'Y/W/w'))%>%
    filter(!is.na(date))

  
## apply model across prediction surface
  pred_rf <- predict(rf, data = pred_town_eff2, type = "response")
  pred_rf <- pred_rf$predictions[, 2]
  
  
  
  # apply calibration models
  pred_rf_cal <- predict(calibration_model, 
                         data.frame(pred = pred_rf), 
                         type = "response",
                         se=T)
  
 
  
  # add to town-level prediction surface
  pred_er <- bind_cols(pred_town_eff2, 
                       eBird.DP.RF = as.vector(pred_rf_cal$fit), 
                       eBird.DP.RF.SE=as.vector(pred_rf_cal$se.fit)) %>% 
    mutate(eBird.DP.RF = pmin(pmax(eBird.DP.RF, 0), 1),
           eBird.DP.RF.SE=unlist(1/eBird.DP.RF.SE))
  
  
  #store town-level weekly predictions
  ebirdRF_results_town_sf[[j]]<-pred_er
  ebirdRF_results_town[[j]]<- dplyr::select(pred_er,-geometry)
  
  

}


#summarise RF model performance across species
RF_town_assess_sum<-do.call("rbind",RF_town_assess)%>%
  group_by(model)%>%
  summarise_at(vars(-group_cols()),
               list(mean=mean,
                    max=max,
                    min=min,
                    sd=sd)
  )
write.csv(RF_town_assess_sum,paste0("Outputs/RF_perfomance.csv"))

  

save(ebirdRF_results_town,file="Objects and Data/3_DP_predictions.rda")

