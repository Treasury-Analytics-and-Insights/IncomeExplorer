

emtr_reform <- function(
  # System parameters
  Parameters,
  # Family parameters
  Partnered = FALSE,
  wage1_hourly = 16.50,
  Children_ages = c(),
  gross_wage2 = 0,
  hours2 = 0,
  AS_Accommodation_Costs = 0,
  AS_Accommodation_Rent = TRUE,
  AS_Area = 1L,
  # Presentation parameters
  max_wage = 1900,
  steps_per_dollar = 1L,
  weeks_in_year = NULL,
  MFTC_WEP_scaling = NULL,
  pov_thresholds = 0.5,
  bhc_median = 43000,
  ahc_median = 33100) {
  
  # Work out length of the year
  model_year <- Parameters$modelyear
  
  if (is.null(weeks_in_year)){
    weeks_in_year <- wks_in_year(model_year)
  }
  if (is.null(MFTC_WEP_scaling)) {
    MFTC_WEP_scaling <- 1/weeks_in_year
  } else {
    MFTC_WEP_scaling <- MFTC_WEP_scaling / weeks_in_year
  }
  
  # Pull AS MaxRates together
  AS_MaxRate_Mortgage <- matrix(data=c(
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area1,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area2,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area3,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area4,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area1,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area2,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area3,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area4,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area1,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area2,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area3,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area4
  ),  nrow=4, ncol=3)
  
  AS_MaxRate_Rent <- matrix(data=c(
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area1,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area2,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area3,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area4,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area1,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area2,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area3,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area4,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area1,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area2,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area3,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area4
  ),  nrow=4, ncol=3)
  
  # Count and categorise children
  N_kids <- length(Children_ages)
  BS_abating_kids <- sum(Children_ages==1L) * (model_year >= 2020) +
    sum(Children_ages==2L) * (model_year >= 2021)
  
  BS_nonabating_kids <- sum(Children_ages==0L) * (model_year >= 2019)

  # Convert tax and abatement schedules to weekly -------------------------------
  # Note that benefit abatement scales in TAWA parameters are weekly values scaled to annual assuming a 52.2 week year
  # Other amounts are annual values, and weekly values are obtained by dividing by the number of weeks in a year,
  # except where the legislation specifies otherwise.
  convert_scale_to_weekly <- function(Scale, wks_in_year = 52L) {
    Scale_Weekly <- Scale
    Scale_Weekly[, 1] <- Scale_Weekly[, 1] / wks_in_year
    return(Scale_Weekly)
  }
  
  Tax_BaseScale_Weekly <-
    convert_scale_to_weekly(Parameters$Tax_BaseScale, weeks_in_year)
  Benefits_SPS_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_SPS_AbatementScale, 52.2)
  Benefits_JSS_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_JSS_AbatementScale, 52.2)
  Benefits_JSS_CoupleAbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_JSS_CoupleAbatementScale, 52.2)
  FamilyAssistance_Abatement_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$FamilyAssistance_Abatement_AbatementScale, 365/7)
  IETC_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$IETC_AbatementScale, weeks_in_year)
  IWTC_PhaseInScale_Single_Weekly<-
    convert_scale_to_weekly(Parameters$FamilyAssistance_IWTC_PhaseIn_Single, 52.2)
  IWTC_PhaseInScale_Couple_Weekly<-
    convert_scale_to_weekly(Parameters$FamilyAssistance_IWTC_PhaseIn_Couple, 52.2)  
  FamilyAssistance_BestStart_Abatement_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$FamilyAssistance_BestStart_Abatement_AbatementScale, 365/7)

  
  
  # Convert rates to weekly------------------------------------------------------
  ACC_max_Weekly <- Parameters$ACC_MaxLeviableIncome / weeks_in_year
  FTC_eldest <- Parameters$FamilyAssistance_FTC_Rates_FirstChild / (365/7)
  FTC_subsequent <- Parameters$FamilyAssistance_FTC_Rates_SubsequentChild / (365/7)
  IWTC_first3 <- Parameters$FamilyAssistance_IWTC_Rates_UpTo3Children / 52L
  IWTC_subsequent <- Parameters$FamilyAssistance_IWTC_Rates_SubsequentChildren / 52L
  IETC_rate <- Parameters$IETC_PerYear / weeks_in_year
  IETC_minimum_income <- Parameters$IETC_MinimalIncome / weeks_in_year
  BS_Rate0 <- Parameters$FamilyAssistance_BestStart_Rates_Age0 / (365/7)
  
  BS_Rate1or2 <- Parameters$FamilyAssistance_BestStart_Rates_Age1or2 / (365/7)
  
  WE_Couple_or_Deps_amount <- Parameters$Benefits_WinterEnergy_Rates_CoupleOrDeps*
    MFTC_WEP_scaling
  
  WE_Single_amount <- Parameters$Benefits_WinterEnergy_Rates_Single*
    MFTC_WEP_scaling
  
  MFTC_amount <- Parameters$FamilyAssistance_MFTC_Rates_MinimumIncome / 52L
  
  # Calculate inverse thresholds for the tax system (weekly)
  NIT <- c(0)
  for (Row in 2:nrow(Tax_BaseScale_Weekly)){
    NIT <- c(NIT, tail(NIT, 1) +
               (Tax_BaseScale_Weekly[Row, 1] - Tax_BaseScale_Weekly[Row - 1, 1]) *
               (1 - Tax_BaseScale_Weekly[Row - 1, 2]))
  }
  
  # Assign benefit rates and abatement schedules ------------------------
  if (Partnered == TRUE) {
    # Couple family 
    if (N_kids == 0L) {
      # No kids 
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_Couple
      Benefit2_Net_0hrs <- Parameters$Benefits_JSS_Rate_Couple
      Benefit_Abatement_Scale <- Benefits_JSS_CoupleAbatementScale_Weekly
    } else {
      # At least one kid
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_CoupleParent
      Benefit2_Net_0hrs <- Parameters$Benefits_JSS_Rate_CoupleParent
      Benefit_Abatement_Scale <- Benefits_JSS_CoupleAbatementScale_Weekly
    }
    
  } else {
    # Single family 
    Benefit2_Net_0hrs <- 0
    if (N_kids == 0) {
      # No kids
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_Single
      Benefit_Abatement_Scale <- Benefits_JSS_AbatementScale_Weekly
      
    } else {
      # At least one kid
      if (min(Children_ages) < Parameters$Benefits_Entitlement_Age_SPS_ChildLowerBound) {
        Benefit1_Net_0hrs <- Parameters$Benefits_SPS_Rate
        Benefit_Abatement_Scale <- Benefits_SPS_AbatementScale_Weekly
      } else {
        # Special case for sole parent whose youngest children is older than
        # Benefits_Entitlement_Age_SPS_ChildLowerBound so the rate is based on JSS,
        # But AbatementScale is based on SPS
        Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_SoleParent
        Benefit_Abatement_Scale <- Benefits_SPS_AbatementScale_Weekly # NB!
      }
      
    }
  }
  
  # Assign AS abatement point, entry threshold, and maximum --------------------
  if (N_kids > 0L & !Partnered) {
    AS_Abate_Point <- 
      ceiling(weeks_in_year *
                Abatement_Vanishing_Point(Benefits_JSS_AbatementScale_Weekly, 
                                          Parameters$Benefits_JSS_Rate_SoleParent))/weeks_in_year
  }else{
    AS_Abate_Point <-
      ceiling(weeks_in_year * 
              Abatement_Vanishing_Point(Benefit_Abatement_Scale,
                                        Benefit1_Net_0hrs))/weeks_in_year
  }
  
  AS_entry_threshold <- 
    ifelse(AS_Accommodation_Rent,
           Parameters$Accommodation_BaseRateThreshold_Rent,
           Parameters$Accommodation_BaseRateThreshold_Mortgage) *
    ((Benefit1_Net_0hrs + Benefit2_Net_0hrs) + (N_kids > 0L) * FTC_eldest)
  
  
  if (AS_Accommodation_Rent){
    AS_Maximum <-
      AS_MaxRate_Rent[AS_Area, pmax(2L - N_kids - 1L * Partnered, 0L) + 1L]
  } else {
    AS_Maximum <-
      AS_MaxRate_Mortgage[AS_Area, pmax(2L - N_kids - 1L * Partnered, 0L) + 1L]
  }
  
  # Initiate the output table --------------------------------------------------
  X <- data.table(gross_wage1 = seq(0, max_wage, 1 / steps_per_dollar))
  X[, hours1 := gross_wage1 / wage1_hourly]
  X[, gross_wage1_annual := weeks_in_year * gross_wage1]
  
  # Partner wage ---------------------------------------------------------------
  X[, gross_wage2 := gross_wage2]

  # These are zero by default --------------------------------------------------
  X[, wage2_tax := 0]
  X[, wage2_ACC_levy := 0]
  X[, net_wage2 := 0]
  X[, net_benefit2 := 0]
  X[, gross_benefit2 := 0]

  X[, IETC_abated1 := 0]
  X[, IETC_abated2 := 0]

  # Abate benefit --------------------------------------------------------------
  X[, net_benefit1 := 
      Abate(TRUE, Benefit1_Net_0hrs, 
            Benefit_Abatement_Scale, gross_wage1 + gross_wage2)]

  
  
  if (Partnered){
    X[, net_benefit2 := Abate(TRUE, Benefit2_Net_0hrs, Benefit_Abatement_Scale,
                              gross_wage1+gross_wage2)]
  }
  
  # Assign IWTC ----------------------------------------------------------------
  # Parameters$FamilyAssistance_IWTC_Eligibility
  #  0 : hours test and do not give to beneficiaries
  #  1 : income test
  #  2 : phase in 
  if (Parameters$FamilyAssistance_IWTC_Eligibility == 1){
    # Use income test 
    
    # Assign income test based on single/couple
    if (Partnered == TRUE){
      IWTC_IncomeThreshold <- Parameters$FamilyAssistance_IWTC_IncomeThreshold_Couple/52.2
    } else {
      IWTC_IncomeThreshold <- Parameters$FamilyAssistance_IWTC_IncomeThreshold_Single/52.2
    }
    
    # Eligible if have children and wage exceeds the threshold
    X[, IWTC_eligible := 1L * (N_kids > 0L) * ((gross_wage1 + gross_wage2) >=
                                                 IWTC_IncomeThreshold)]
    
    # Calculate unabated IWTC
    X[, IWTC_unabated := IWTC_eligible * (IWTC_first3 + max(0L, N_kids - 3L) *
                                            IWTC_subsequent)]
    
    # Parameters$FamilyAssistance_IWTC_ToBeneficiaries determines if beneficiaries are eligible for IWTC
    # 0 : Not eligible
    # 1 : Eligible
    if (Parameters$FamilyAssistance_IWTC_ToBeneficiaries == 0){
      # If IWTC eligible, zero out benefit
      X[IWTC_unabated>0, 
        `:=`(net_benefit1 = 0,  net_benefit2 = 0)]
    }
    
    # tidy up
    X[, IWTC_eligible := NULL]
    
  } else if (Parameters$FamilyAssistance_IWTC_Eligibility == 2){
    # Use phase-in method 
    
    # Phase in scale
    if (Partnered == TRUE){
      IWTC_PhaseInScale_Weekly <- IWTC_PhaseInScale_Couple_Weekly
    } else {
      IWTC_PhaseInScale_Weekly <- IWTC_PhaseInScale_Single_Weekly
    }
    # Maximum amount
    IWTC_max <- IWTC_first3 + max(0L, N_kids - 3L) * IWTC_subsequent
    
    # Calcuated phased-in/unabated amount
    X[, IWTC_unabated := (N_kids > 0L)*
        pmin(IWTC_max,
             Abate(TRUE, 0, IWTC_PhaseInScale_Weekly, gross_wage1+gross_wage2))]
  } else {
    # Hours test and do not give to beneficiaries
    
    # Hours threshold
    if (Partnered == TRUE){ 
      FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Couple
      
    } else {
      FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Single
    }
    
    X[, IWTC_eligible := 1L * (N_kids > 0L) * ((hours1 + hours2) >= FullTimeWorkingHours)]
    
    # Calculate unabated IWTC
    X[, IWTC_unabated := IWTC_eligible * (IWTC_first3 + max(0L, N_kids - 3L) *
                                            IWTC_subsequent)]
    
    # If receiving IWTC, zero out benefit
    X[IWTC_unabated >0, 
      `:=`(net_benefit1 = 0, net_benefit2 = 0)]
    
    # tidy up
    X[, IWTC_eligible := NULL]
  }
  
  # Back out Gross benefit
  X[, gross_benefit1 := Gross_From_Net(net_benefit1, NIT, Tax_BaseScale_Weekly)]
  X[, gross_benefit1 := Gross_From_Net(net_benefit1, NIT, Tax_BaseScale_Weekly)]

  
  if (Partnered){
    X[, gross_benefit2 := Gross_From_Net(net_benefit2, NIT, Tax_BaseScale_Weekly)]
  }
  
  # Add wage on to benefit and tax
  X[, gross_benefit_and_wage1 := gross_benefit1 + gross_wage1]
  X[, net_benefit_and_wage1 := Net_From_Gross(gross_benefit_and_wage1, Tax_BaseScale_Weekly)]
  X[, wage1_tax := (gross_benefit_and_wage1 - net_benefit_and_wage1) - 
      (gross_benefit1 - net_benefit1)]
  
  if (Partnered) {
    X[, gross_benefit_and_wage2 := gross_benefit2 + gross_wage2]
    X[, net_benefit_and_wage2 := Net_From_Gross(gross_benefit_and_wage2, Tax_BaseScale_Weekly)]
    X[, wage2_tax := (gross_benefit_and_wage2 - net_benefit_and_wage2) - (gross_benefit2 - net_benefit2)]
  }
  
  # Work out ACC levy
  X[, wage1_ACC_levy := pmin(gross_wage1,ACC_max_Weekly)*Parameters$ACC_LevyRate2]
  
  if (Partnered){
    X[, wage2_ACC_levy := pmin(gross_wage2,ACC_max_Weekly)*Parameters$ACC_LevyRate2]
  }
  
  # Form net wage
  X[, net_wage1 := gross_wage1 - wage1_tax - wage1_ACC_levy]
  
  if (Partnered){
    X[, net_wage2 := gross_wage2 - wage2_tax - wage2_ACC_levy]
  }
  
  # Form unabated FTC
  X[, FTC_unabated := (N_kids>0L)*FTC_eldest+max(N_kids-1L,0L)*FTC_subsequent]
  
  
  
  # Hours test for MFTC ------------
  
  # Hours threshold
  if (Partnered == TRUE){ 
    FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Couple
    
  } else {
    FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Single
  }
  
  
  X[, MFTC_eligible :=  (N_kids > 0L) &
      ((hours1 + hours2) >= FullTimeWorkingHours) &
      ((net_benefit1 + net_benefit2) == 0)]
  
  
  # Work out MFTC amount (if benefit == 0)
  X[, MFTC := MFTC_eligible *
      (pmax(MFTC_amount - (gross_wage1 + gross_wage2 - wage1_tax - wage2_tax),0))]
  
  
  # tidy up
  X[, MFTC_eligible := NULL]
  
  
  # Abate FTC and IWTC ------------
  X[, AbateAmount :=
      Apply(gross_wage1 + gross_wage2 + gross_benefit1 + gross_benefit2,
            FamilyAssistance_Abatement_AbatementScale_Weekly)]
  
  
  # If FamilyAssistance_Abatement_Order has not been defined
  # then define it with default value: 0
  # So old parameter files should still work 
  if(is.null(Parameters$FamilyAssistance_Abatement_Order)){
    Parameters$FamilyAssistance_Abatement_Order <- 0
  }
  
  # Changing the order FTC and IWTC abatement ---------------------
  if(Parameters$FamilyAssistance_Abatement_Order == 0){
    # Abate FTC first then IWTC
    X[, FTC_abated := pmax(0, FTC_unabated - AbateAmount)]
    X[, RemainingAbatement := pmax(0, AbateAmount - FTC_unabated)]
    X[, IWTC_abated := pmax(0, IWTC_unabated - RemainingAbatement)]
  } else{
    # Abate IWTC first then FTC
    X[, IWTC_abated := pmax(0, IWTC_unabated - AbateAmount)]
    X[, RemainingAbatement := pmax(0, AbateAmount - IWTC_unabated)]
    X[, FTC_abated := pmax(0, FTC_unabated - RemainingAbatement)]
  }
  
  # tidy up
  X[, RemainingAbatement := NULL]
  
  # If receiving best start they shouldn’t be able to receive IETC ---------------
  # Best Start ------------
  X[, BestStart_Universal := BS_nonabating_kids*BS_Rate0]
  X[, BestStart_Abated := Abate(TRUE, BS_abating_kids*BS_Rate1or2,
                                FamilyAssistance_BestStart_Abatement_AbatementScale_Weekly,
                                gross_wage1+gross_wage2+gross_benefit1+gross_benefit2)]
  X[, BestStart_Total := BestStart_Universal + BestStart_Abated]
  
  
  # Work out IETC -----------
  X[, IETC_eligible1 := (net_benefit1 == 0) & (FTC_abated == 0) &
      (IWTC_abated == 0) & (MFTC == 0) & (BestStart_Total == 0) &
      (gross_wage1 >= IETC_minimum_income)]
  
  X[IETC_eligible1 == TRUE, 
    IETC_abated1 := Abate(TRUE, IETC_rate, IETC_AbatementScale_Weekly, 
                          gross_wage1)]
  
  
  X[, IETC_eligible2 := (net_benefit2 == 0) & (FTC_abated == 0) & 
      (IWTC_abated == 0) & (MFTC == 0) & (BestStart_Total == 0) & 
      (gross_wage2 >= IETC_minimum_income)]
  
  
  X[IETC_eligible2 == TRUE, 
    IETC_abated2 := Abate(TRUE, IETC_rate, IETC_AbatementScale_Weekly, gross_wage2)]
   
  
  X[Parameters$IETC_OnlyFamiliesWithoutChildren == TRUE & N_kids > 0L, 
    ':=' (IETC_abated1 = 0,
          IETC_abated2 = 0)]
  
  # tidy up
  X[, IETC_eligible1 := NULL]
  X[, IETC_eligible2 := NULL]
  
  
  
  
  # Winter Energy --------
  X[, WE_Couple_or_Deps := 0]
  X[, WE_Single := 0]
  
  X[, WE_eligible := (net_benefit1 + net_benefit2) > 0]
  
  X[, WE_Couple_or_Deps_eligible := WE_eligible == TRUE & 
      (N_kids > 0L | Partnered)]

  X[WE_Couple_or_Deps_eligible == TRUE,
    WE_Couple_or_Deps := WE_Couple_or_Deps_amount]

  X[, WE_Single_eligible :=  WE_eligible == TRUE & 
      ((!Partnered) & N_kids == 0L)]

  X[WE_eligible == TRUE & WE_Single_eligible == TRUE,
    WE_Single := WE_Single_amount]
  
  X[, WinterEnergy := WE_Couple_or_Deps +  WE_Single]

  # tidy up
  X[, WE_eligible := NULL]
  X[, WE_Couple_or_Deps := NULL]
  X[, WE_Couple_or_Deps_eligible := NULL]
  X[, WE_Single := NULL]
  X[, WE_Single_eligible := NULL]
  
  # Accommodation Supplement -----------
  X[, AS_Amount := pmax(
    pmin(Parameters$Accommodation_PaymentPercentage * (AS_Accommodation_Costs - AS_entry_threshold),
         AS_Maximum) -
      pmax(gross_wage1 + gross_wage2 - AS_Abate_Point, 0) * 
      Parameters$Accommodation_AbatementRate * ((net_benefit1 + net_benefit2) == 0), 0)]
  
  # Net income and EMTR
  X[, Net_Income := net_benefit1 + net_wage1 + net_benefit2 + net_wage2 +
      IETC_abated1 + IETC_abated2 + FTC_abated + IWTC_abated + MFTC +  
      WinterEnergy + BestStart_Total + AS_Amount]
  
  X[, EMTR := 1 - steps_per_dollar*(shift(Net_Income,1L,type="lead")-Net_Income)]
  X[, EMTR := zoo::na.locf(EMTR)]
  
  # Replacement rate
  X[, Replacement_Rate := first(Net_Income) / Net_Income]
  X[, Participation_Tax_Rate := 1 - (Net_Income - first(Net_Income))/(gross_wage1 + gross_wage2)]
  
  # As in TAWA proc the disposable income is calculated as: 
  # P_Income_Total + P_FamilyAssistance_Total + P_TaxCredit_IETC - P_Income_TaxPayable - P_ACC_LevyPayable
  # Same definition as "Net_Income", so use "Net_Income" as disposable income.
  
  # Number of children under 14
  LT_14 <- sum(Children_ages < 14)
  
  # Number of children over 14
  GTE_14 <- sum( Children_ages >= 14) + fifelse(Partnered == FALSE, 1, 2)
  
  # Modified OECD
  moecd_eq_factor <- 1 + 0.5 * (GTE_14 - 1) + 0.3 * (LT_14)
  
  # Equivalised Income
  X[, Equivalised_Income := Net_Income / moecd_eq_factor]

  # BHC Depth
  X[, BHC_Depth := (moecd_eq_factor * (pov_thresholds*bhc_median - pmax(0, Equivalised_Income*weeks_in_year)))/weeks_in_year]
 
  # After housing cost disposable income
  X[, AHC_Net_Income := Net_Income - AS_Accommodation_Costs]
  
  # Equivalised Income
  X[, AHC_Equivalised_Income := AHC_Net_Income / moecd_eq_factor]
  
  # AHC Depth
  X[, AHC_Depth := (moecd_eq_factor * (pov_thresholds*ahc_median - pmax(0, AHC_Equivalised_Income*weeks_in_year)))/weeks_in_year]
  
  # Unequivalised median
  X[, ":=" (BHC_Unequiv_Poverty_Line = pov_thresholds * moecd_eq_factor * bhc_median,
            AHC_Unequiv_Poverty_Line = pov_thresholds * moecd_eq_factor * ahc_median,
            Eq_Factor = moecd_eq_factor)]
  
  return(X)
}


