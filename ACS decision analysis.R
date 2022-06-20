#install.packages("decisionSupport")
library(decisionSupport)

# Internal function to run the model line by line

make_variables<-function(est, n = 1)
{
  x<-random(rho = est, n = n)
  for(i in colnames(x))
    assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(estimate_read_csv("acis_inputs_EN.csv"))

acis_costbenefit <- function(x, varnames){
  
  
  # The interventions include:
  # Intervention 1: Weather station-SMS-gender
  # Intervention 2: SMS-gender
  # Intervention 3: SMS-loudspeaker
  # Intervention 4: Paper-loudspeaker
  
  # Risks, costs and benefits include: 
  # i1: risk/cost/benefit variable incurred for intervention 1
  # i2: risk/cost/benefit variable incurred for intervention 2
  # i3: risk/cost/benefit variable incurred for intervention 3
  # i4: risk/cost/benefit variable incurred for intervention 4
  # i12: risk/cost/benefit variable incurred for intervention 1 and intervention 2
  # i234: risk/cost/benefit for intervention 2, intervention3 and intervention 4
  # i34: risk/cost/benefit for intervention 3 and intervention 4
  # i1234: risk/cost/benefit for all intervention 1, intervention 2, intervention 3 and intervention 4
  
  # Risks that impact benefits
  # inaccurate forecasts:  risk which is on-going every season/year
  # weather risks: risk which is on-going every season/year
  
  #1. Calculating uncertainties and risks####
  
  # Drought risk for each year
  drought_risk_i1234 <- chance_event(chance_drought_i1234,
            value_if = 1, 
            n = n_years)
  
  # Chance of having inaccurate drought forecast at the beginning of season intervention 1, 2, 3, 4
  inaccurate_forecast_extreme_drought_i1234 <- chance_event(chance_inaccurate_forecast_extreme_drought_i1234,
            value_if = 1, 
            n = n_years)
  
  # Extreme cold risk for each year
  risk_extreme_cold <- chance_event(chance_extreme_cold,
            value_if = 1, 
            n = n_years)
  
  # Chance of having inaccurate extreme cold forecast 
  inaccurate_forecast_extreme_cold_i1234 <- chance_event(chance_inaccurate_forecast_extreme_cold_i1234,
            value_if = 1, 
            n = n_years)
  
  # Chance of having inaccurate weekly weather forecast, intervention 1
  inaccurate_forecast_i1 <- chance_event(chance_inaccurate_forecast_i1,
            value_if = 1,
            n = n_years)
  
  # Chance of having inaccurate weekly weather forecast, intervention 2, 3, 4
  inaccurate_forecast_i234 <- chance_event(chance_inaccurate_forecast_i234,
            value_if = 1, 
            n = n_years)
  
  # Risk of having events can cause re-fertilize
  risk_refertilize_i1234 <- chance_event(chance_refertilize_i1234,
            value_if = 1, 
            n = n_years)
  
  # Risk of re_sow due to extreme events (rain, cold) 
  risk_resow_i1234<-chance_event(chance_resow_i1234,
            value_if = 1, 
            n = n_years)
  
  # 2. Calculating costs####
  
  # 2.1 Forecast generation####
  # Set up new local meteorological station for the whole project period (Intervention 1)
  cost_new_met_station <- rep(0, n_years)
  
  cost_new_met_station[1] <- (met_station_esta_i1 + 
            vv(met_station_main_i1, 
            var_CV, 1) + 
            cost_forecasts_access_i1)/exchange_rate
  
  cost_new_met_station[2:5] <- vv(met_station_main_i1,
            var_CV, 4)/exchange_rate
  
  # Buy forecast from provincial meteorological station for the whole project period 
  # (Intervention1234)
  cost_forecast_province <- rep(0, n_years)
  
  cost_forecast_province[1:5] <- vv(cost_weekly_forecasts_i1234 +
            cost_seasonal_forecasts_i1234, 
            var_CV, n_years)/exchange_rate
  #2.2 Translation####
  
  # Translation from forecasts (training on translation) to advisory costs 
  #for the whole project period (Intervention1234)
  cost_translation <- rep(0, n_years)
  
  cost_translation[1:2] <- vv(cost_cb_translation_staff_i1234, 
            var_CV,2)*cb_translation_n12/exchange_rate
  
  cost_translation[3:5] <- vv(cost_cb_translation_staff_i1234, 
            var_CV,3)*cb_translation_n345/exchange_rate
  #2.3 Transfer####
  # Calculating total households and farm households in 5 years
  # https://socratic.org/questions/how-do-you-calculate-population-growth
  time <- 1:n_years
  
  total_households_i1234 <- vv(baseline_households_i1234, 
            var_CV, n_years)*exp(household_increase_rate*time)
  
  total_farm_households_i1234 <- vv(baseline_farm_households_i1234, var_CV, n_years)*
            exp(household_increase_rate*time)
  
  # Transfer and communication costs: district and commune/village for 
  #the whole project period (Intervention1234)
  
  cost_capacity_communication<-rep(0,n_years)
  
  cost_capacity_communication[1]<-(vv(cost_cb_commune_i1234, var_CV,1)*n_communes+
            vv(leftlet_year1_i1234, var_CV,1)*vv(total_farm_households_i1234, var_CV,1)+
            vv(village_meeting_launch_i1234, var_CV,1)*n_village+
            vv(video_i1234_peryear, var_CV,1)+
            vv(cost_print_seasonal_bulletinA0_i1234, var_CV,1)*
            (n_village*n_per_village+n_communes)*n_print_per_year)/exchange_rate
    
  cost_capacity_communication[2:5]<-(
            vv(video_i1234_peryear, var_CV,4)+
            vv(cost_print_seasonal_bulletinA0_i1234,var_CV,4)*
            (n_village*n_per_village+n_communes)*n_print_per_year)/exchange_rate
    
  # Cost for sending SMS to rice farmers for the whole project period (Intervention12)
  cost_rice_SMS<- rep(0, n_years)
  
  cost_rice_SMS[1:5]<-vv(cost_per_SMS_i123, var_CV,5)*
            vv(messages_per_time_rice_i123, var_CV,5)*
            vv(number_times_per_year_rice_i123, var_CV,5)*
            total_farm_households_i1234/exchange_rate
      
  cost_animal_SMS<-rep(0,n_years)
  cost_animal_SMS[1:5]<-vv(cost_per_SMS_i123,var_CV,5)*
            vv(messages_per_time_animal_i123,var_CV,5)*
            vv(number_times_per_year_animal_i123, var_CV,5)*
            vv(percent_animal_households_i1234,var_CV,5)*
            vv(total_farm_households_i1234, var_CV,5)/exchange_rate
       
  # Cost for sending SMS to village leader for the whole project period(Intervention3)
  cost_SMS_villageleader<- rep(0, n_years)
  
  cost_SMS_villageleader[1:5]<-vv(cost_per_SMS_i123, var_CV,5)*
            vv(messages_per_time_rice_i123, var_CV,5)*
            vv(number_times_per_year_rice_i123,var_CV,5)*
            commune_village_SMS_i3/exchange_rate+
            vv(cost_per_SMS_i123, var_CV,5)*
            vv(messages_per_time_animal_i123, var_CV,5)*
            vv(number_times_per_year_animal_i123, var_CV,5)*
            commune_village_SMS_i3/exchange_rate 
       
  # Allowance for village leaders for the whole project period (Intervention34)
  cost_allowance_village_leader<-rep(0,n_years)
  
  cost_allowance_village_leader[1:5]<-n_village*
            vv(allowance_village_leader_permonth_loud_i34, var_CV,5)*
            months_per_year/exchange_rate
    
  # Cost_collect_bulletin (Intervention4)
  cost_collect_bulletin<-rep(0,n_years)
  
  cost_collect_bulletin[1:5]<-(allowance_bulletin_collect_time_short_i4*
            vv(percent_short_distance_i4, var_CV,5)*
            times_per_month*months_per_year*n_village)/exchange_rate+
            (allowance_bulletin_collect_time_long_i4*
            vv(1-percent_short_distance_i4,var_CV,5)*
            times_per_month*months_per_year*n_village)/exchange_rate 
     
  # 2.4 Support the use and learning####
     
  # Establish demonstration models (5000m2/season, 2models of 1ha/year) 
  # Total fertilizer cost per ha (Intervention1234)
  
  fa_adv_cost_perha <-(NPK5105_advice_i1234*NPK5105_price_i1234+
            N_advice_i1234*N_price_i1234+
            K_advice_i1234*K_price_i1234)/exchange_rate
  
  # Consider the partial support from the Government to develop demonstration model (5000m2~1/2ha)
  cost_per_modeli1234<-((seed_advice_i1234*price_seed_i1234+
            plant_protection_support_i1234)/exchange_rate+
            fa_adv_cost_perha*percent_fertilizer_model_supporti1234)/2
      
  cost_model<-rep(0, n_years)
  cost_model[1:5]<-(model_training_i1234*n_training+no_model_compare_i1234*
            cost_per_modeli1234+model_monitor_i1234+field_visit_i1234)/exchange_rate
   
  # 2.5 Cross-cutting and ME costs####
     
  # Cost for gender intervention social action and analysis SAA (training, dialogue, community events)
  # for the whole project period (Intervention12)
  
  cost_gender<-rep(0,n_years)
  
  cost_gender[1]<-(SAA_TOT+dialogue_village*
              n_dialogues*n_village+
              cost_community_event*n_village)/exchange_rate 
  cost_gender[2:5]<-0
  
  # Monitoring ####
  # Monitoring and Evaluation for the whole project period (Intervention1234)
  
  ME_cost<-rep(0,n_years)
  
  ME_cost[0:2]<-0
  ME_cost[3]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
  ME_cost[5]<-(ME_district_i1234+ME_commune_i1234*n_communes)/exchange_rate
    
  # Total cost for intervention 1####
  
  total_cost_i1<-cost_new_met_station+cost_forecast_province+
              cost_translation+cost_capacity_communication+cost_rice_SMS+
              cost_animal_SMS+cost_model+cost_gender+ME_cost
  # Total cost for intervention 2####
  total_cost_i2<-cost_forecast_province+cost_translation+
              cost_capacity_communication+cost_rice_SMS+cost_animal_SMS+
              cost_model+cost_gender+ME_cost
    
  # Total cost for intervention 3####
  total_cost_i3<-cost_forecast_province+cost_translation+
              cost_capacity_communication+cost_SMS_villageleader+
              cost_allowance_village_leader+cost_model+ME_cost
                                  
  # Total cost for intervention 4####
  total_cost_i4<-cost_forecast_province+cost_translation+
              cost_capacity_communication+cost_collect_bulletin+
              cost_allowance_village_leader+cost_model+ME_cost
                                              
  # 3 Calculating benefits####
  #3.1 Economic benefits####
  #3.1.1 Rice benefits####
  #for all interventions in the condition of having good weather forecast
  #3.1.1.1 Advisories to cope with extreme drought####
    
  # Drought risk for each year
  # drought_risk_i1234<-chance_event(chance_drought_i1234,value_if = 1, n=n_years)
    
  # Drought area for each year 
  rice_drought_i1234<-rep(0,n_years)
  rice_drought_i1234[1:5]<-vv(rice_area_drought_i1234, var_CV, 5)*
              drought_risk_i1234
    
  # Rice area (total area of two seasons a year) that are not affected by drought 
  rice_area_effect<-rep(0,n_years)
  rice_area_effect[1:5]<-vv(total_rice_area_i1234,var_CV, 5)-
              vv(rice_drought_i1234,var_CV, 5)
    
  # Avoid losses due to drought for the whole project period
  reduced_drought_losses<-rep(0, n_years)
  reduced_drought_losses[1:5]<-vv(reduce_loss_drought_i1234,var_CV, 5)*
              vv(rice_drought_i1234,var_CV_high, 5)/exchange_rate
  
  # 3.1.1.2 Seed benefits####
    
  # Seed dose reduction per ha
  seed_dose_reduction_perha<-rep(0,n_years)
       
  seed_dose_reduction_perha[1:5]<-((seed_baseline_i1234-seed_applied_i1234)*
              vv(price_seed_i1234, var_CV, 5)+vv(labor_pick_i1234,var_CV, 5))/exchange_rate
    
  # Write the function for adoption, if adoption reaches to the peak,use saturated rate 
  adoptionrate<-function(p,q,n_years,dis_adoption,r_saturated)
       # p: adoption rate in year 1
       # q: annual adoption rate due to personal effect
       {
         r<-rep(0, n_years)
         for (t in c(2:n_years))
         {r[1]<-p
         r[t]<-r[t-1]+q*r[t-1]-r[t-1]*dis_adoption}
         ifelse(r>r_saturated,r_saturated,r)
       }
       
  # Total seed dose reduction benefit intervention 1
  # Sowing adoption rate intervention 1
  seed_rate_i1<-adoptionrate(rate_seed_inno_i1,
              rate_seed_imitation_i1, n_years,
              dis_adoption_i123,rate_saturated_i12)
  # seed benefit=dose benefit((farmer dose-advisory does)*advised times) + 
  # times benefit(farmer dose*reduced times)
    
  # Benefit from dose reduction intervention 1####
  benefit_dose_seed_i1<-rep(0,n_years)
  benefit_dose_seed_i1[1:5]<-seed_dose_reduction_perha*
              rice_area_effect*
              seed_rate_i1*effective_rate 
    
  # Total seed dose reduction benefit intervention 2
  # Sowing adoption rate intervention 2
       
  seed_rate_i2<-adoptionrate(rate_seed_inno_i2,
              rate_seed_imitation_i2,n_years,
              dis_adoption_i123,rate_saturated_i12)
       
  # Benefit from dose reduction intervention 2   
  benefit_dose_seed_i2<-rep(0,n_years)
  benefit_dose_seed_i2[1:5]<-seed_dose_reduction_perha*
              rice_area_effect*
              seed_rate_i2*
              effective_rate
    
  # Total seed dose reduction benefit intervention 3 
  # sowing adoption rate intervention 3
  seed_rate_i3<-adoptionrate(rate_seed_inno_i3,
              rate_seed_imitation_i3,n_years,
              dis_adoption_i123,rate_saturated_i34)
  # benefit from dose reduction   
  
  benefit_dose_seed_i3<-rep(0,n_years)
  
  benefit_dose_seed_i3[1:5]<-seed_dose_reduction_perha*
              rice_area_effect*
              seed_rate_i3*
              effective_rate
  # Total seed dose reduction benefit intervention 4 
  # sowing adoption rate intervention 4
  
  seed_rate_i4<-adoptionrate(rate_seed_inno_i4,
              rate_seed_imitation_i4, n_years,
              dis_adoption_i4,rate_saturated_i34)
  
  # benefit from dose reduction   
  benefit_dose_seed_i4<-rep(0,n_years)
  
  benefit_dose_seed_i4[1:5]<-seed_dose_reduction_perha*
              rice_area_effect*
              seed_rate_i4*
              effective_rate
      
  # Benefits from reduced times of re-sowing####
  # Refer to risk of re_sow due to extreme events (rain, cold) if business as usual 
  # risk_resow_i1234<-chance_event(chance_resow_i1234,value_if = 1, n=n_years)
  # taking into consideration of the chance to reduce the risk
  resow_reduced_i1234<-risk_resow_i1234*chance_resow_advice_i1234
       
  # Benefit for times reduction in sowing for intervention 1 
  # Costs for sowing for 1 ha per year (equal to costs can be reduced if it can avoid)
  # No need to *2 for dose as the area is already*for two seasons
  
  reduce_resow_costsperha_i1234<-(seed_baseline_i1234)*
              vv(price_seed_i1234, var_CV, 5)
       
  inaccurate_forecast_i1<-chance_event(chance_inaccurate_forecast_i1,value_if = 1,n=n_years)
  
  benefit_time_seed_i1<-rep(0,n_years)
       
  benefit_time_seed_i1[1:5]<-resow_reduced_i1234*rice_area_effect*
              (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
              vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
              seed_rate_i1*effective_rate/exchange_rate
           
  # Benefit for times reduction in sowing for intervention 2 
  inaccurate_forecast_i234<-chance_event(chance_inaccurate_forecast_i234,value_if = 1, n=n_years)
  
  benefit_time_seed_i2<-rep(0,n_years)
  
  benefit_time_seed_i2[1:5]<-resow_reduced_i1234*
              rice_area_effect*
              (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
              vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
              seed_rate_i2*effective_rate/exchange_rate
       
  # Benefit for times reduction in sowing for intervention 3 
  
  benefit_time_seed_i3<-rep(0,n_years)
  
  benefit_time_seed_i3[1:5]<-resow_reduced_i1234*
            rice_area_effect*
            (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
            vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
            seed_rate_i3*effective_rate/exchange_rate
  # Benefit for times reduction in sowing for intervention 4 
  
  benefit_time_seed_i4<-rep(0,n_years)
  benefit_time_seed_i4[1:5]<-resow_reduced_i1234*
          rice_area_effect*
          (reduce_resow_costsperha_i1234+labor_seed_fertilize_i1234)*
          vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
          seed_rate_i4*effective_rate/exchange_rate
      
  # 3.1.1.3 Fertilizer benefits####
  # Dose reduction####
  # for fertilizer per season per ha
       
  benefit_dose_fer_perha<-((NPK5105_baseline_i1234 -NPK5105_applied_i1234)*
          NPK5105_price_i1234+
          (N_baseline_i1234-N_applied_i1234)*N_price_i1234+
          (K_baseline_i1234-K_applied_i1234)*K_price_i1234)/exchange_rate
       
  # Benefit for dose reduction for intervention 1 
  # fertilizer and plant protection adoption rate 
  rate_fer_pla_i1<- adoptionrate(rate_fer_pla_inno_i1,
          rate_fer_pla_immi_i1,n_years,
          dis_adoption_i123,rate_saturated_i12)
    
  # Benefit - For area, it needs to be discounted with potential area destroyed by 
  # annual severe risks such as hailstones/flashfloods
  
  benefit_dose_fer_i1<-rep(0,n_years)
       
  benefit_dose_fer_i1[1:5]<-benefit_dose_fer_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))* 
            rate_fer_pla_i1*
            effective_rate
        
  # Benefit for dose reduction for intervention 2
  # fertilizer and plant protection adoption rate 
  
  rate_fer_pla_i2<-adoptionrate(rate_fer_pla_inno_i2,
            rate_fer_pla_immi_i2,n_years,
            dis_adoption_i123,rate_saturated_i12)
  # benefit
  
  benefit_dose_fer_i2<-rep(0,n_years)
  
  benefit_dose_fer_i2[1:5]<-benefit_dose_fer_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i2*
            effective_rate
        
  # Benefit for dose reduction for intervention 3 
  # fertilizer and plant protection adoption rate 
  rate_fer_pla_i3<-adoptionrate(rate_fer_pla_inno_i3,
            rate_fer_pla_immi_i3,n_years,
            dis_adoption_i123,rate_saturated_i34)
  # benefit
  
  benefit_dose_fer_i3<-rep(0,n_years)
       
  benefit_dose_fer_i3[1:5]<-benefit_dose_fer_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i3*
            effective_rate
        
  # Benefit for dose reduction for intervention 4    
  # fertilizer and plant protection adoption rate 
  
  rate_fer_pla_i4<-adoptionrate(rate_fer_pla_inno_i4,
            rate_fer_pla_immi_i4,n_years,
            dis_adoption_i4,rate_saturated_i34)
  # benefit
  
  benefit_dose_fer_i4<-rep(0,n_years)
  
  benefit_dose_fer_i4[1:5]<-benefit_dose_fer_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i4*
            effective_rate
       
             
  # Benefit for fertilizing times reduction#### 
  # Refer to risk of having events can cause re-fertilize
  # risk_refertilize_i1234<-chance_event(chance_refertilize_i1234,value_if = 1, n=n_years)
  # taking into consideration of the chance to reduce the risk 
  
  refertilize_reduced_i1234<-risk_refertilize_i1234*
            chance_refertilize_advice_i1234
       
  # cost for fertilizing for one ha using farmers' dose
       
  fa_fer_cost_perha<-(NPK5105_baseline_i1234*NPK5105_price_i1234+
            N_baseline_i1234*N_price_i1234+
            K_baseline_i1234*K_price_i1234)/exchange_rate
    
  # Benefit for times reduction for intervention 1 
  
  benefit_time_fer_i1<-rep(0,n_years)
  
  benefit_time_fer_i1[1:5]<-refertilize_reduced_i1234*rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
            vv(resow_fer_area_percentage_i1234,var_CV_high,5)*
            rate_fer_pla_i1*
            effective_rate
       
  # Benefit for times reduction for intervention 2 
  
  benefit_time_fer_i2<-rep(0,n_years)
  
  benefit_time_fer_i2[1:5]<-refertilize_reduced_i1234*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
            vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
            rate_fer_pla_i2*
            effective_rate
      
  # Benefit for times reduction for intervention 3
  
  benefit_time_fer_i3<-rep(0,n_years)
  
  benefit_time_fer_i3[1:5]<-refertilize_reduced_i1234*rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
            vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
            rate_fer_pla_i3*
            effective_rate
           
  # Benefit for times reduction for intervention 4
  
  benefit_time_fer_i4<-rep(0,n_years)
  
  benefit_time_fer_i4[1:5]<-refertilize_reduced_i1234*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            (fa_fer_cost_perha+labor_seed_fertilize_i1234/exchange_rate)*
            vv(resow_fer_area_percentage_i1234, var_CV_high,5)*
            rate_fer_pla_i4*
            effective_rate
            
  # 3.1.1.4. Plant protection benefits####
     
  #Farmers use many different types of pesticides with different names so the calculation will base
  # on the potential time reduced in 5 years/1000m2 and the average cost for each time 
  # Benefit effective combined dose and timing in applying plant protection substances 
  # Reduced time sprayed per year per ha. This is not only due to weather but also due to
  # knowledge, behaviour and access to information to local sale agent. Therefore, chance to reduce
  # is calculated for all and already considered affected area and inaccurate forecasts
      
  reduce_cost_spray_peryear_i1234<-vv(reduced_times_spray_i1234, var_CV_high,5)*
            vv(reduced_cost_per_time_spray_i1234,var_CV_high,5)
  
  increase_cost_monitor_peryear_i1234<-vv(times_monitor_increased, var_CV_high,5)*
            vv(cost_monitor_pest_increased,var_CV_high,5)
  
  change_cost_spray_perha<- (reduce_cost_spray_peryear_i1234-increase_cost_monitor_peryear_i1234)*10
      
  # Benefit for times reduction for intervention 1 
  
  benefit_time_spray_i1<-rep(0,n_years)
  
  benefit_time_spray_i1[1:5]<-change_cost_spray_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i1*effective_rate/exchange_rate
           
  # Benefit for times reduction for intervention 2
  
  benefit_time_spray_i2<-rep(0,n_years)
  
  benefit_time_spray_i2[1:5]<-change_cost_spray_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i2*effective_rate/exchange_rate
           
  # Benefit for times reduction for intervention 3
  
  benefit_time_spray_i3<-rep(0,n_years)
  
  benefit_time_spray_i3[1:5]<-change_cost_spray_perha*
            rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i3*effective_rate/exchange_rate
           
  # Benefit for times reduction for intervention 4
  
  benefit_time_spray_i4<-rep(0,n_years)
  
  benefit_time_spray_i4[1:5]<-change_cost_spray_perha*rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            rate_fer_pla_i4*effective_rate/exchange_rate
    
  #3.1.1.5. Rice yield benefits#### 
  
  # Rice yield benefit for intervention 1
  
  rice_benefit_change_i1<-rep(0,n_years)
  
  rice_benefit_change_i1[1:5]<-rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            vv(yield_change_i1234, var_CV, 5)*rice_price*
            ((seed_rate_i1+2*rate_fer_pla_i1)/3)*effective_rate/exchange_rate
  # Rice yield benefit for intervention 2
  
  rice_benefit_change_i2<-rep(0,n_years)
  
  rice_benefit_change_i2[1:5]<-rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            vv(yield_change_i1234, var_CV, 5)*
            rice_price*
            ((seed_rate_i2+2*rate_fer_pla_i2)/3)*effective_rate/exchange_rate
  # Rice yield benefit for intervention 3
  
  rice_benefit_change_i3<-rep(0,n_years)
  
  rice_benefit_change_i3[1:5]<-rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            vv(yield_change_i1234, var_CV, 5)*
            rice_price*
            ((seed_rate_i3+2*rate_fer_pla_i3)/3)*effective_rate/exchange_rate
  
  # Rice yield benefit for intervention 4
  
  rice_benefit_change_i4<-rep(0,n_years)
  
  rice_benefit_change_i4[1:5]<-rice_area_effect*
            (1-vv(rice_area_loss_severe_risks_i1234, var_CV,5))*
            vv(yield_change_i1234, var_CV, 5)*
            rice_price*
            ((seed_rate_i4+2*rate_fer_pla_i4)/3)*effective_rate/exchange_rate
  # Discounting uncertainty of forecasts-accurate good weather forecast####    
  # Total benefit for rice: This benefit equal to total rice benefit in the 
  # good weather forecasts conditions discounting the inaccurate weather forecasts. 
  # When having negative effect. It does not normally affect all but just a part of that. For example, farmers do not need
  # to put all fertilizer back
  # Total benefit for rice intervention 1
  #Refer to risks
  #inaccurate_forecast_i1<-chance_event(chance_inaccurate_forecast_i1,value_if = 1,n=n_years)
  #inaccurate_forecast_i234<-chance_event(chance_inaccurate_forecast_i234,value_if = 1, n=n_years)
  #inaccurate_forecast_extreme_drought_i1234<-chance_event(chance_inaccurate_forecast_extreme_drought_i1234,value_if = 1, n=n_years)
    
  total_rice_i1<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
            (inaccurate_forecast_extreme_drought_i1234*
            rice_profit_no_drought*
            rice_drought_i1234/exchange_rate)+
            benefit_dose_seed_i1+benefit_dose_fer_i1+
            (benefit_time_seed_i1+benefit_time_fer_i1)*(1-inaccurate_forecast_i1)-
            (benefit_time_seed_i1+benefit_time_fer_i1)*inaccurate_forecast_i1+
            benefit_time_spray_i1+
            rice_benefit_change_i1*(1-inaccurate_forecast_i1)-
            rice_benefit_change_i1*inaccurate_forecast_i1
    
  # Total benefit for rice intervention 2
  
  total_rice_i2<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
            (inaccurate_forecast_extreme_drought_i1234*
            rice_profit_no_drought*
            rice_drought_i1234/exchange_rate)+
            benefit_dose_seed_i2+benefit_dose_fer_i2+
            (benefit_time_seed_i2+benefit_time_fer_i2)*
            (1-inaccurate_forecast_i234)-
            (benefit_time_seed_i2+benefit_time_fer_i2)*inaccurate_forecast_i234+
            benefit_time_spray_i2+
            rice_benefit_change_i2*(1-inaccurate_forecast_i234)-
            rice_benefit_change_i2*inaccurate_forecast_i234
  
  # Total benefit for rice intervention 3
  
  total_rice_i3<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
          (inaccurate_forecast_extreme_drought_i1234*
          rice_profit_no_drought*
          rice_drought_i1234/exchange_rate)+
          benefit_dose_seed_i3+benefit_dose_fer_i3+
          (benefit_time_seed_i3+benefit_time_fer_i3)*(1-inaccurate_forecast_i234)-
          (benefit_time_seed_i3+benefit_time_fer_i3)*inaccurate_forecast_i234+
          benefit_time_spray_i3+
          rice_benefit_change_i3*(1-inaccurate_forecast_i234)-
          rice_benefit_change_i3*inaccurate_forecast_i234
       
  # Total benefit for rice intervention 4
          total_rice_i4<-(1-inaccurate_forecast_extreme_drought_i1234)*reduced_drought_losses-
          (inaccurate_forecast_extreme_drought_i1234*rice_profit_no_drought*rice_drought_i1234/exchange_rate)+
          benefit_dose_seed_i4+benefit_dose_fer_i4+
          (benefit_time_seed_i4+benefit_time_fer_i4)*(1-inaccurate_forecast_i234)-
          (benefit_time_seed_i4+benefit_time_fer_i4)*inaccurate_forecast_i234+
          benefit_time_spray_i4+
          rice_benefit_change_i4*(1-inaccurate_forecast_i234)-
          rice_benefit_change_i4*inaccurate_forecast_i234
    
  # 3.1.2 Animal husbandry benefits#### 
  # 3.1.2.1 Buffalo benefits#### 
  # risk of extreme cold that can affect buffaloes and cows. Advice can be provided
  # in any extreme cold events
  
  risk_extreme_cold<-chance_event(chance_extreme_cold,value_if = 1, n=n_years)
    
  # Adoption rate for animal husbandry advice
    
  animal_rate_i1<-adoptionrate(rate_ani_inno_i1,
          rate_ani_immi_i1,n_years,
          dis_adoption_i123,
          rate_saturated_i12)
      
  # Inaccurate forecast are same for all interventions
  inaccurate_forecast_extreme_cold_i1234<-chance_event(chance_inaccurate_forecast_extreme_cold_i1234,
          value_if = 1, 
          n=n_years)
    
  # Benefit for buffalo intervention 1
  
  buffalo_benefiti1<-rep(0, n_years)
  
  buffalo_benefiti1[1:5]<-(risk_extreme_cold*
          vv(total_buffalo_i1234,var_CV_high,5)*
          vv(price_buffalo_i1234,var_CV_high,5)*
          vv(reduced_death_animal_i1,var_CV_high,5)*
          animal_rate_i1*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
     
  # Benefit for buffalo intervention 2
  animal_rate_i2<-adoptionrate(rate_ani_inno_i2,
          rate_ani_immi_i2,n_years,
          dis_adoption_i123,
          rate_saturated_i12)
    
  #benefit for buffalo
    
  buffalo_benefiti2<-rep(0, n_years)
  
  buffalo_benefiti2[1:5]<-(risk_extreme_cold*
          vv(total_buffalo_i1234,var_CV_high,5)*
          vv(price_buffalo_i1234,var_CV_high,5)*
          vv(reduced_death_animal_i2,var_CV_high,5)*
          animal_rate_i2*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
    
  # Benefit for buffalo intervention 3
    
  animal_rate_i3<-adoptionrate(rate_ani_inno_i3,
          rate_ani_immi_i3,n_years,
          dis_adoption_i123,
          rate_saturated_i34)
  
  #benefit for buffalo
  
  buffalo_benefiti3<-rep(0, n_years)
  
  buffalo_benefiti3[1:5]<-(risk_extreme_cold*
          vv(total_buffalo_i1234,var_CV_high,5)*
          vv(price_buffalo_i1234,var_CV_high,5)*
          vv(reduced_death_animal_i3,var_CV_high,5)*
          animal_rate_i3*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
  
  # Benefit for buffalo intervention 4
  
  animal_rate_i4<-adoptionrate(rate_ani_inno_i4,
          rate_ani_immi_i4,n_years,
          dis_adoption_i4,
          rate_saturated_i34)
    
  #benefit for buffalo
  
  buffalo_benefiti4<-rep(0, n_years)
  
  buffalo_benefiti4[1:5]<-(risk_extreme_cold*
          vv(total_buffalo_i1234,var_CV_high,5)*
          vv(price_buffalo_i1234,var_CV_high,5)*
          vv(reduced_death_animal_i4,var_CV_high,5)*
          animal_rate_i4*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
  # 3.1.2.2 Cow benefits####
  # Benefit for cow intervention 1
  
  cow_benefiti1<-rep(0, n_years)
  
  cow_benefiti1[1:5]<-(risk_extreme_cold*
          vv(total_cow_i1234,var_CV_high,5)*
          vv(price_cow_i1234,var_CV_high,5)*
          vv(reduced_death_animal_i1,var_CV_high,5)*
          animal_rate_i1*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
    
  # Benefit for cow intervention 2
  
  cow_benefiti2<-rep(0, n_years)
  
  cow_benefiti2[1:5]<-(risk_extreme_cold*
           vv(total_cow_i1234,var_CV_high,5)*
           vv(price_cow_i1234,var_CV_high,5)*
           vv(reduced_death_animal_i2,var_CV_high,5)*
           animal_rate_i2*effective_rate/exchange_rate)*
          (1-2*inaccurate_forecast_extreme_cold_i1234)
  
  # Benefit for cow intervention 3
  
  cow_benefiti3<-rep(0, n_years)
  
  cow_benefiti3[1:5]<-(risk_extreme_cold*
           vv(total_cow_i1234,var_CV_high,5)*
           vv(price_cow_i1234,var_CV_high,5)*
           vv(reduced_death_animal_i3,var_CV_high,5)*
           animal_rate_i3*effective_rate/exchange_rate)*
           (1-2*inaccurate_forecast_extreme_cold_i1234)
  
  # Benefit for cow intervention 4
  
  cow_benefiti4<-rep(0, n_years)
  
  cow_benefiti4[1:5]<-(risk_extreme_cold*
           vv(total_cow_i1234,var_CV_high,5)*
           vv(price_cow_i1234,var_CV_high,5)*
           vv(reduced_death_animal_i4,var_CV_high,5)*
           animal_rate_i4*effective_rate/exchange_rate)*
           (1-2*inaccurate_forecast_extreme_cold_i1234)
        
  # 3.2 Gender impacts####
  # Benefit for gender impacts intervention 1
  
  gender_benefiti1<-rep(0,n_years)
  
  gender_benefiti1[1]<-0
  
  gender_benefiti1[2:5]<-(vv(new_income_farm_peryear_i12,var_CV_high,4)*
          vv(rate_farm,var_CV_high,4)*
          total_farm_households_i1234[2:5]+
          vv(new_income_nonfarm_peryear_i12, var_CV_high,4)*
          vv(rate_nonfarm,var_CV_high,4)*
          total_farm_households_i1234[2:5])*
          gender_coverage/exchange_rate
      
  # Benefit for gender impacts intervention 2
  
  gender_benefiti2<-gender_benefiti1
    
  # Benefit for gender impacts intervention 3
  
  gender_benefiti3<-rep(0,n_years)
  gender_benefiti3[1:5]<-0
    
  # Benefit for gender impacts intervention 4
  gender_benefiti4<-rep(0,n_years)
  gender_benefiti4[1:5]<-0
    
  # 3.3 Environmental impacts####
  # Benefit on environment of intervention 1
  # Fish benefits####
  #Farmers who apply plant protection advices will not harm the fishes
  
  fish_benefiti1<-rep(0,n_years)
  
  fish_benefiti1[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
          vv(lost_fish_i1234,var_CV_high,5)*
          vv(chance_reduced_risk_fish_death, var_CV_high,5)*
          rate_fer_pla_i1*
          effective_rate/exchange_rate
  # Water benefits####
  
  water_benefiti1<-rep(0,n_years)
  
  water_benefiti1[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
          vv(total_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i1*
          effective_rate/exchange_rate
  # Total environmental benefit intervention 1 
  
  env_benefiti1<-fish_benefiti1+water_benefiti1
    
  # Benefit on environment of intervention 2
    
  fish_benefiti2<-rep(0,n_years)
  
  fish_benefiti2[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
          vv(lost_fish_i1234,var_CV_high,5)*
          vv(chance_reduced_risk_fish_death, var_CV_high,5)*
          rate_fer_pla_i2*
          effective_rate/exchange_rate
  # Water benefits
  
  water_benefiti2<-rep(0,n_years)
  
  water_benefiti2[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
          vv(total_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i2*
          effective_rate/exchange_rate
  
  # Total environmental benefit intervention 2
  
  env_benefiti2<-fish_benefiti2+water_benefiti2
    
    
  # Benefit on environment of intervention 3
  # Fish benefits
  
  fish_benefiti3<-rep(0,n_years)
  
  fish_benefiti3[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
          vv(chance_reduced_risk_fish_death, var_CV_high,5)*
          vv(lost_fish_i1234,var_CV_high,5)*
          rate_fer_pla_i3*
          effective_rate/exchange_rate
  # Water benefits
  
  water_benefiti3<-rep(0,n_years)
  
  water_benefiti3[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
          vv(total_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i3*
          effective_rate/exchange_rate
  
  # Total environmental benefit intervention 2 
  env_benefiti3<-fish_benefiti3+water_benefiti3
    
  # Benefit on environment of intervention 4
  # Fish benefits
  
  fish_benefiti4<-rep(0,n_years)

  fish_benefiti4[1:5]<-vv(area_pond_impacted_i1234,var_CV_high,5)*
          vv(chance_reduced_risk_fish_death, var_CV_high,5)*
          vv(lost_fish_i1234,var_CV_high,5)*
          rate_fer_pla_i4*effective_rate/exchange_rate
  
  # Water benefits
  water_benefiti4<-rep(0,n_years)
  
  water_benefiti4[1:5]<-vv(reduced_water_expenditure_i1234,var_CV_high,5)*
          vv(total_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i4*effective_rate/exchange_rate
  
  # Total environmental benefit 4
  env_benefiti4<-fish_benefiti4+water_benefiti4
    
  # 3.4 Health impacts####
    
  # Benefit on health of intervention 1
  # Reduced expenditure on health
  
  health_impacti1<-rep(0,n_years)
  
  health_impacti1[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
          vv(total_farm_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i1*
          effective_rate/exchange_rate)
    
  # Total health impact intervention 1
  health_impacti1
    
  # Benefit on health of intervention 2
  
  health_impacti2<-rep(0,n_years)
  
  health_impacti2[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
          vv(total_farm_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i2*effective_rate/exchange_rate)
    
  # Total health impact intervention 2
  health_impacti2
    
    
  # Benefit on health of intervention 3
  # Reduced expenditure on health
  health_impacti3<-rep(0,n_years)
  health_impacti3[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
          vv(total_farm_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i3*effective_rate/exchange_rate)
    
  # Total health impact intervention 3
  health_impacti3
    
  # Benefit on health of intervention 4
  # Reduced expenditure on health
  
  health_impacti4<-rep(0,n_years)
  
  health_impacti4[1:5]<-(vv(reduced_expenditure_health_i1234/5,var_CV_high,5)*
          vv(total_farm_households_i1234,var_CV,5)*
          vv(percent_pollution_reduction_i1234,var_CV_high,5)*
          rate_fer_pla_i4*effective_rate/exchange_rate)
    
  # Total health impact intervention 4
  health_impacti4
    
  # 3.5 GHG emission reduction####
  #GHG reduction intervention 1
  
  GHG_impactsi1<-rep(0,n_years)
  
  GHG_impactsi1[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
           vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
           vv(total_rice_area_i1234,var_CV, 5)*
           vv(carbon_price,var_CV_high,5)*
           rate_fer_pla_i1*effective_rate/exchange_rate
  #GHG reduction intervention 2
  
  GHG_impactsi2<-rep(0,n_years)
  
  GHG_impactsi2[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
            vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
            vv(total_rice_area_i1234,var_CV, 5)*
            vv(carbon_price,var_CV_high,5)*
            rate_fer_pla_i2*effective_rate/exchange_rate
  #GHG reduction intervention 3
  
  GHG_impactsi3<-rep(0,n_years)
  
  GHG_impactsi3[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
            vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
            vv(total_rice_area_i1234,var_CV, 5)*
            vv(carbon_price,var_CV_high,5)*
            rate_fer_pla_i3*effective_rate/exchange_rate
  #GHG reduction intervention 4
  
  GHG_impactsi4<-rep(0,n_years)
  
  GHG_impactsi4[1:5]<-(vv(methan_reduction_co2eq,var_CV_high,5)+
             vv(nito_oxide_reduction_co2eq,var_CV_high,5))*
             vv(total_rice_area_i1234,var_CV, 5)*
             vv(carbon_price,var_CV_high,5)*
             rate_fer_pla_i4*effective_rate/exchange_rate
    
  # 4. Total benefit intervention 1 (Provincial People's Committee)####
  
  total_benefiti1<-total_rice_i1+buffalo_benefiti1+cow_benefiti1+env_benefiti1+
  gender_benefiti1+health_impacti1+GHG_impactsi1
    
  # Annual bottom-line benefit intervention 1
  
  bottomline_benefiti1=total_benefiti1-total_cost_i1
  
  # 4.1 Benefits aggregated for stakeholders####
  
  # 4.1.1 Benefits for Hydro-Met Station####
  hydromet_benefit_i1<-cost_new_met_station+cost_forecast_province
  
  # 4.1.2 Benefits for Provincial Department of Agriculture and Rural Development####
  pdard_benefit_i1<-cost_translation
  
  # 4.1.3 Benefits for Agricultural Service Centre####
  asc_benefit_i1<-cost_capacity_communication+cost_model+ME_cost
  
  # 4.1.4 Benefits for SMS service providers####
  SMS_provider_benefit_i1<-cost_rice_SMS+cost_animal_SMS
  
  # 4.1.5 Benefits for seed suppliers####
  
  seed_supplier_benefit_i1<- 0-(benefit_dose_seed_i1+
    (benefit_time_seed_i1)*(1-inaccurate_forecast_i1)-
    (benefit_time_seed_i1)*inaccurate_forecast_i1+
    benefit_time_spray_i1)
   
  
  # 4.1.6 Benefits for fertilizer suppliers####
  
  fertilizer_supplier_benefit_i1<- 0-(benefit_dose_fer_i1+
    (benefit_time_fer_i1)*(1-inaccurate_forecast_i1)-
    (benefit_time_fer_i1)*inaccurate_forecast_i1)
  
  # 4.1.7 Benefits for plant protection suppliers####
  plant_protection_supplier_benefit_i1<- 0-(benefit_time_spray_i1)
  
  # 4.1.8 Benefit for women's union/LNGO####
  wu_ngo_benefit_i1<-cost_gender
  
  # 4.1.9 Benefits for rice farmers####
  rice_fa_benefit_i1<-total_rice_i1+(water_benefiti1*total_farm_households_i1234/total_households_i1234)+
    gender_benefiti1+health_impacti1
  # 4.1.9.1 Average benefits for one rice household####
  one_rice_HH1<-rice_fa_benefit_i1/total_farm_households_i1234
  
  #4.1.10 Benefits for animal husbandry farmers####
  
  animal_fa_benefiti1<-buffalo_benefiti1+cow_benefiti1
  
  # 4.1.10.1 Average benefits for one animal husbandry household####
  one_annimal_HH1<-animal_fa_benefiti1/(percent_animal_households_i1234*total_farm_households_i1234)
  
  #4.1.11 Benefits for fish farmers####
  fi_fa_benefit_i1<-fish_benefiti1
  
  # 4.1.12 Benefits for public####
  
  public_benefit_i1<-water_benefiti1*
    (total_households_i1234-total_farm_households_i1234)/total_households_i1234
  
  # NPV intervention 1 for stakeholders####
  # 4.2 NPV of overall intervention- Provincial People's Committee####
  NPV1<-discount(bottomline_benefiti1, discount_rate, calculate_NPV = TRUE)
  # 4.2.1 NPV for Provincial Hydro-met Station####
  NPV_Hydromet1<-discount(hydromet_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.2 NPV for Provincial Department of Agriculture and Rural Development####
  NPV_DARD1<-discount(pdard_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.3 NPV for Agricultural Service Centre####
  NPV_ASC1<-discount(asc_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.4 NPV for SMS service providers####
  NPV_SMSP1<-discount(SMS_provider_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.5 NPV for seed suppliers####
  NPV_SS1<-discount(seed_supplier_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.6 NPV for fertilizer suppliers####
  NPV_FS1<-discount(fertilizer_supplier_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.7 NPV for plant protection suppliers ####
  NPV_PPS1<-discount(plant_protection_supplier_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.8 Benefit for women's union/Local NGO####
  NPV_WU_NGO1<-discount(wu_ngo_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.9 NPV for rice farmers####
  NPV_Rice1<-discount(rice_fa_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.10 NPV for animal husbandry farmers####
  NPV_AH1<-discount(animal_fa_benefiti1, discount_rate, calculate_NPV = TRUE)
  #4.2.11 NPV for fish farmers####
  NPV_Fish1<-discount(fi_fa_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.12 NPV for the public####
  NPV_Public1<-discount(public_benefit_i1, discount_rate, calculate_NPV = TRUE)
  #4.2.9.1 Average NPV for one rice household####
  NPV_One_RiceHH1<-discount(one_rice_HH1, discount_rate, calculate_NPV = TRUE)
  #4.2.10.1 Average NPV for one animal raising household####
  NPV_One_AnimalHH1<-discount(one_annimal_HH1, discount_rate, calculate_NPV = TRUE)
  
 
  #5. Total benefit intervention 2 (Provincial People's Committee####
  total_benefiti2<-total_rice_i2+buffalo_benefiti2+cow_benefiti2+env_benefiti2+
  gender_benefiti2+health_impacti2+GHG_impactsi2
  
  # Annual bottom-line benefit intervention 2
  bottomline_benefiti2=total_benefiti2-total_cost_i2
  
  # 5.1 Benefits aggregated for stakeholders####
  
  #5.1.1 Benefits for Hydro-Met Station####
  hydromet_benefit_i2<-cost_forecast_province
  
  # 5.1.2 Benefits for Provincial Department of Agriculture and Rural Development####
  pdard_benefit_i2<-cost_translation
  
  # 5.1.3 Benefits for Agricultural Service Centre####
  asc_benefit_i2<-cost_capacity_communication+cost_model+ME_cost
  
  # 5.1.4 Benefits for SMS service providers####
  SMS_provider_benefit_i2<-cost_rice_SMS+cost_animal_SMS
  
  # 5.1.5 Benefits for seed suppliers ####
  
  seed_supplier_benefit_i2<- 0-(benefit_dose_seed_i2+
                                  (benefit_time_seed_i2)*(1-inaccurate_forecast_i234)-
                                  (benefit_time_seed_i2)*inaccurate_forecast_i234+
                                  benefit_time_spray_i2)
  
  
  # 5.1.6 Benefits for fertilizer suppliers####
  
  fertilizer_supplier_benefit_i2<- 0-(benefit_dose_fer_i2+
                                        (benefit_time_fer_i2)*(1-inaccurate_forecast_i234)-
                                        (benefit_time_fer_i2)*inaccurate_forecast_i234)
  
  # 5.1.7 Benefits for plant protection suppliers####
  plant_protection_supplier_benefit_i2<- 0-(benefit_time_spray_i2)
  
  # 5.1.8 Benefits for women's union/LNGO####
  wu_ngo_benefit_i2<-cost_gender
  
  # 5.1.9 Benefits for rice farmers####
  rice_fa_benefit_i2<-total_rice_i2+(water_benefiti2*total_farm_households_i1234/total_households_i1234)+
    gender_benefiti2+health_impacti2
  
  # 5.1.9.1 Average benefits for one rice household####
  one_rice_HH2<-rice_fa_benefit_i2/total_farm_households_i1234
  
  #5.1.10 Benefits for animal husbandry farmers####
  
  animal_fa_benefiti2<-buffalo_benefiti2+cow_benefiti2
  
  #5.1.10.1 Average benefits for one animal husbandry household####
  one_annimal_HH2<-animal_fa_benefiti2/(percent_animal_households_i1234*total_farm_households_i1234)
  
  #5.1.11 Benefits for fish farmers####
  fi_fa_benefit_i2<-fish_benefiti2
  
  #5.1.12 Benefits for the public####
  
  public_benefit_i2<-water_benefiti2*
    (total_households_i1234-total_farm_households_i1234)/total_households_i1234
    
  # NPV intervention 2 for stakeholders####
  #5.2 NPV of overall intervention- Provincial People's Committee####
  NPV2<-discount(bottomline_benefiti2, discount_rate, calculate_NPV = TRUE)
  #5.2.1 NPV for Hydro-Met Station####
  NPV_Hydromet2<-discount(hydromet_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.2 NPV for Provincial Department of Agriculture and Rural Development####
  NPV_DARD2<-discount(pdard_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.3 NPV for Agricultural Service Centre####
  NPV_ASC2<-discount(asc_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.4 NPV for SMS service providers####
  NPV_SMSP2<-discount(SMS_provider_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.5 NPV for seed suppliers####
  NPV_SS2<-discount(seed_supplier_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.6 NPV for fertilizer suppliers####
  NPV_FS2<-discount(fertilizer_supplier_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.7 NPV for plant protection suppliers####
  NPV_PPS2<-discount(plant_protection_supplier_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.8 NPV for women's union/Local NGO####
  NPV_WU_NGO2<-discount(wu_ngo_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.9 NPV for rice farmers####
  NPV_Rice2<-discount(rice_fa_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.10 NPV for animal husbandry farmers####
  NPV_AH2<-discount(animal_fa_benefiti2, discount_rate, calculate_NPV = TRUE)
  #5.2.11 NPV for fish farmers####
  NPV_Fish2<-discount(fi_fa_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.12 NPV for the public####
  NPV_Public2<-discount(public_benefit_i2, discount_rate, calculate_NPV = TRUE)
  #5.2.9.1 Average NPV for one rice household####
  NPV_One_RiceHH2<-discount(one_rice_HH2, discount_rate, calculate_NPV = TRUE)
  #5.2.10.1 Average NPV for one animal raising household####
  NPV_One_AnimalHH2<-discount(one_annimal_HH2, discount_rate, calculate_NPV = TRUE)

  # 6. Total benefits intervention 3####
  total_benefiti3<-total_rice_i3+buffalo_benefiti3+cow_benefiti3+env_benefiti3+
  +gender_benefiti3+health_impacti3+GHG_impactsi3
  
  # Annual bottom-line benefit intervention 3
  bottomline_benefiti3=total_benefiti3-total_cost_i3
  
  # 6.1 Benefits aggregated for stakeholders####
  
  # 6.1.1 Benefits for Hydro-Met Station####
  hydromet_benefit_i3<-cost_forecast_province
  
  # 6.1.2 Benefits for Provincial Provincial Department of Agriculture and Rural Development####
  pdard_benefit_i3<-cost_translation
  
  # 6.1.3 Benefits for Agricultural Service Centre####
  asc_benefit_i3<-cost_capacity_communication+cost_model+ME_cost
  
  # 6.1.4 Benefits for SMS service providers####
  SMS_provider_benefit_i3<-cost_SMS_villageleader
  
  # 6.1.5 Benefits for seed suppliers####
  
  seed_supplier_benefit_i3<- 0-(benefit_dose_seed_i3+
                                  (benefit_time_seed_i3)*(1-inaccurate_forecast_i234)-
                                  (benefit_time_seed_i3)*inaccurate_forecast_i234+
                                  benefit_time_spray_i3)
  
  
  # 6.1.6 Benefits for fertilizer suppliers####
  
  fertilizer_supplier_benefit_i3<- 0-(benefit_dose_fer_i3+
                                        (benefit_time_fer_i3)*(1-inaccurate_forecast_i234)-
                                        (benefit_time_fer_i3)*inaccurate_forecast_i234)
  
  # 6.1.7 Benefits for plant protection suppliers####
  plant_protection_supplier_benefit_i3<- 0-(benefit_time_spray_i3)
  
  # 6.1.8 Benefits for village leaders####
  village_leader_benefit_i3<-cost_allowance_village_leader
  
  # 6.1.9 Benefits for women's union/LNGO####
  wu_ngo_benefit_i3<-0
  
  # 6.1.10 Benefits for rice farmers####
  rice_fa_benefit_i3<-total_rice_i3+(water_benefiti3*total_farm_households_i1234/total_households_i1234)+
    gender_benefiti3+health_impacti3
  
  # 6.1.10.1 Average benefits for one rice household####
  one_rice_HH3<-rice_fa_benefit_i3/total_farm_households_i1234
  
  #6.1.11 Benefits for animal husbandry farmers####
  
  animal_fa_benefiti3<-buffalo_benefiti3+cow_benefiti3
  # 6.1.11.1 Average benefits for one animal husbandry household####
  one_annimal_HH3<-animal_fa_benefiti3/(percent_animal_households_i1234*total_farm_households_i1234)
  
  # 6.1.12 Benefits for fish farmers####
  fi_fa_benefit_i3<-fish_benefiti3
  
  # 6.1.13 Benefits for the public####
  
  public_benefit_i3<-water_benefiti3*
    (total_households_i1234-total_farm_households_i1234)/total_households_i1234
  
  # NPV intervention 3####
  #6.2 NPV of overall intervention- Provincial People's Committee####
  NPV3<-discount(bottomline_benefiti3, discount_rate, calculate_NPV = TRUE)
  #6.2.1 NPV for Hydro-Met Station####
  NPV_Hydromet3<-discount(hydromet_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.2 NPV for Provincial Department of Agriculture and Rural Development####
  NPV_DARD3<-discount(pdard_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.3 NPV for Agricultural Service Centre####
  NPV_ASC3<-discount(asc_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.4 NPV for SMS service providers####
  NPV_SMSP3<-discount(SMS_provider_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.5 NPV for seed suppliers####
  NPV_SS3<-discount(seed_supplier_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.6 NPV for fertilizer suppliers####
  NPV_FS3<-discount(fertilizer_supplier_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.7NPV for plant protection suppliers####
  NPV_PPS3<-discount(plant_protection_supplier_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.8NPV for village leaders####
  NPV_VL3<-discount(village_leader_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.9NPV for women's union/Local NGO####
  NPV_WU_NGO3<-discount(wu_ngo_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.10 NPV for rice farmers####
  NPV_Rice3<-discount(rice_fa_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.11 NPV for animal husbandry farmers####
  NPV_AH3<-discount(animal_fa_benefiti3, discount_rate, calculate_NPV = TRUE)
  #6.2.12 NPV for fish farmers####
  NPV_Fish3<-discount(fi_fa_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.13 NPV for the public####
  NPV_Public3<-discount(public_benefit_i3, discount_rate, calculate_NPV = TRUE)
  #6.2.10.1 Average NPV for one rice household####
  NPV_One_RiceHH3<-discount(one_rice_HH3, discount_rate, calculate_NPV = TRUE)
  #6.2.10.2 Average NPV for one animal raising household####
  NPV_One_AnimalHH3<-discount(one_annimal_HH3, discount_rate, calculate_NPV = TRUE)
    
  # 7. Total benefits intervention 4####
  total_benefiti4<-total_rice_i4+buffalo_benefiti4+cow_benefiti4+env_benefiti4+
  gender_benefiti4+health_impacti4+GHG_impactsi4
  
  # Annual bottom-line benefit intervention 4
  bottomline_benefiti4=total_benefiti4-total_cost_i4
    
  # 7.1 Benefits aggregated for stakeholders####
  
  # 7.1.1 Benefits for Hydro-Met Station####
  hydromet_benefit_i4<-cost_forecast_province
  
  # 7.1.2 Benefits for Provincial Department of Agriculture and Rural Development####
  pdard_benefit_i4<-cost_translation
  
  # 7.1.3 Benefits for Agricultural Service Centre####
  asc_benefit_i4<-cost_capacity_communication+cost_model+ME_cost
  
  # 7.1.4 Benefits for SMS service providers####
  SMS_provider_benefit_i4<-0
  
  # 7.1.5 Benefits for seed suppliers####
  
  seed_supplier_benefit_i4<- 0-(benefit_dose_seed_i4+
                                  (benefit_time_seed_i4)*(1-inaccurate_forecast_i234)-
                                  (benefit_time_seed_i4)*inaccurate_forecast_i234+
                                  benefit_time_spray_i4)
  
  
  # 7.1.6 Benefits for fertilizer suppliers####
  
  fertilizer_supplier_benefit_i4<- 0-(benefit_dose_fer_i4+
                                        (benefit_time_fer_i4)*(1-inaccurate_forecast_i234)-
                                        (benefit_time_fer_i4)*inaccurate_forecast_i234)
  
  # 7.1.7 Benefits for plant protection suppliers####
  plant_protection_supplier_benefit_i4<- 0-(benefit_time_spray_i4)
  
  # 7.1.8 Benefits for village leaders####
  village_leader_benefit_i4<-cost_collect_bulletin+cost_allowance_village_leader
  # 7.1.9 Benefits for women's union/LNGO####
  wu_ngo_benefit_i4<-0
  
  # 7.1.10 Benefits for rice farmers####
  rice_fa_benefit_i4<-total_rice_i4+(water_benefiti4*total_farm_households_i1234/total_households_i1234)+
    gender_benefiti4+health_impacti4
  
  # 7.1.10.1 Average for one rice household####
  one_rice_HH4<-rice_fa_benefit_i4/total_farm_households_i1234
  
  #7.1.11 Benefits for animal husbandry farmers####
  
  animal_fa_benefiti4<-buffalo_benefiti4+cow_benefiti4
  #7.11.1 Average benefits for one animal husbandry household####
  one_annimal_HH4<-animal_fa_benefiti4/(percent_animal_households_i1234*total_farm_households_i1234)
  
  #7.1.12 Benefits for fish farmers####
  fi_fa_benefit_i4<-fish_benefiti4
  
  #7.1.13 Benefits for the public####
  
  public_benefit_i4<-water_benefiti4*
    (total_households_i1234-total_farm_households_i1234)/total_households_i1234
  
  # NPV intervention 4####
  #7.2 NPV of overall intervention- Provincial People's Committee
  NPV4<-discount(bottomline_benefiti4, discount_rate, calculate_NPV = TRUE)
  #7.2.1 NPV for Hydro-Met Station
  NPV_Hydromet4<-discount(hydromet_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.2 NPV for Provincial Department of Agriculture and Rural Development
  NPV_DARD4<-discount(pdard_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.3 NPV for Agricultural Service Centre
  NPV_ASC4<-discount(asc_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.4 NPV for SMS service providers
  NPV_SMSP4<-discount(SMS_provider_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.5 NPV for seed suppliers
  NPV_SS4<-discount(seed_supplier_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.6 NPV for fertilizer suppliers
  NPV_FS4<-discount(fertilizer_supplier_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.7 NPV for plant protection suppliers
  NPV_PPS4<-discount(plant_protection_supplier_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.8 NPV for village leaders
  NPV_VL4<-discount(village_leader_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.9 NPV for women's union/Local NGO
  NPV_WU_NGO4<-discount(wu_ngo_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.10 NPV for rice farmers
  NPV_Rice4<-discount(rice_fa_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.11 NPV for animal husbandry farmers
  NPV_AH4<-discount(animal_fa_benefiti4, discount_rate, calculate_NPV = TRUE)
  #7.2.12 NPV for fish farmers
  NPV_Fish4<-discount(fi_fa_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.13 NPV for the public
  NPV_Public4<-discount(public_benefit_i4, discount_rate, calculate_NPV = TRUE)
  #7.2.10.1 Average NPV for one rice household
  NPV_One_RiceHH4<-discount(one_rice_HH4, discount_rate, calculate_NPV = TRUE)
  #7.2.11.1 Average NPV for one animal raising household
  NPV_One_AnimalHH4<-discount(one_annimal_HH4, discount_rate, calculate_NPV = TRUE)
 
  return(list(
          NPV_Intervention1=NPV1,
          NPV_Hydromet_Intervention1=NPV_Hydromet1,
          NPV_DARD_Intervention1=NPV_DARD1,
          NPV_ASC_Intervention1=NPV_ASC1,
          NPV_SMSP_Intervention1=NPV_SMSP1,
          NPV_SS_Intervention1=NPV_SS1,
          NPV_FS_Intervention1=NPV_FS1,
          NPV_PPS_Intervention1=NPV_PPS1,
          NPV_WU_NGO_Intervention1=NPV_WU_NGO1,
          NPV_Rice_Intervention1=NPV_Rice1,
          NPV_AH_Intervention1=NPV_AH1,
          NPV_Fish_Intervention1=NPV_Fish1,
          NPV_Public_Intervention1=NPV_Public1,
          NPV_One_RiceHH_Intervention1=NPV_One_RiceHH1,
          NPV_One_AnimalHH_Intervention1=NPV_One_AnimalHH1,
          NPV_Intervention2=NPV2,
          NPV_Hydromet_Intervention2=NPV_Hydromet2,
          NPV_DARD_Intervention2=NPV_DARD2,
          NPV_ASC_Intervention2=NPV_ASC2,
          NPV_SMSP_Intervention2=NPV_SMSP2,
          NPV_SS_Intervention2=NPV_SS2,
          NPV_FS_Intervention2=NPV_FS2,
          NPV_PPS_Intervention2=NPV_PPS2,
          NPV_WU_NGO_Intervention2=NPV_WU_NGO2,
          NPV_Rice_Intervention2=NPV_Rice2,
          NPV_AH_Intervention2=NPV_AH2,
          NPV_Fish_Intervention2=NPV_Fish2,
          NPV_Public_Intervention2=NPV_Public2,
          NPV_One_RiceHH_Intervention2=NPV_One_RiceHH2,
          NPV_One_AnimalHH_Intervention2=NPV_One_AnimalHH2,
          NPV_Intervention3=NPV3,
          NPV_Hydromet_Intervention3=NPV_Hydromet3,
          NPV_DARD_Intervention3=NPV_DARD3,
          NPV_ASC_Intervention3=NPV_ASC3,
          NPV_SMSP_Intervention3=NPV_SMSP3,
          NPV_SS_Intervention3=NPV_SS3,
          NPV_FS_Intervention3=NPV_FS3,
          NPV_PPS_Intervention3=NPV_PPS3,
          NPV_Village_Leader_Intervention3=NPV_VL3,
          NPV_WU_NGO_Intervention3=NPV_WU_NGO3,
          NPV_Rice_Intervention3=NPV_Rice3,
          NPV_Fish_Intervention3=NPV_Fish3,
          NPV_AH_Intervention3=NPV_AH3,
          NPV_Public_Intervention3=NPV_Public3,
          NPV_One_RiceHH_Intervention3=NPV_One_RiceHH3,
          NPV_One_AnimalHH_Intervention3=NPV_One_AnimalHH3,
          NPV_Intervention4=NPV4,
          NPV_Hydromet_Intervention4=NPV_Hydromet4,
          NPV_DARD_Intervention4=NPV_DARD4,
          NPV_ASC_Intervention4=NPV_ASC4,
          NPV_SMSP_Intervention4=NPV_SMSP4,
          NPV_SS_Intervention4=NPV_SS4,
          NPV_FS_Intervention4=NPV_FS4,
          NPV_PPS_Intervention4=NPV_PPS4,
          NPV_Village_Leader_Intervention4=NPV_VL4,
          NPV_WU_NGO_Intervention4=NPV_WU_NGO4,
          NPV_Rice_Intervention4=NPV_Rice4,
          NPV_AH_Intervention4=NPV_AH4,
          NPV_Fish_Intervention4=NPV_Fish4,
          NPV_Public_Intervention4=NPV_Public4,
          NPV_One_RiceHH_Intervention4=NPV_One_RiceHH4,
          NPV_One_AnimalHH_Intervention4=NPV_One_AnimalHH4
          ))
  }
  
  #Running the model ####
  decisionSupport::decisionSupport("acis_inputs_EN.csv",
  outputPath = paste("MCResults",sep=""),
  welfareFunction = acis_costbenefit,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames")
  # the function might return error when plotting. It might probably due to some NPV returns of 0
  # and therefore, it is not possible to run PLS
  # Another option to run the model with handy results (not many folders but only 
  #the input and output variable simulations)
  #https://cran.r-project.org/web/packages/decisionSupport/vignettes/example_decision_function.html

#mcSimulation_results <- decisionSupport::mcSimulation(
# estimate = decisionSupport::estimate_read_csv("acis_inputs_EN.csv"),
# model_function = acis_costbenefit,
# numberOfModelRuns = 1e4, #run 1,000 times
# functionSyntax = "plainNames")
# write.csv(mcSimulation_results$x, "MCResults\\x.csv")
# write.csv(mcSimulation_results$y, "MCResults\\y.csv")
  
  

  
