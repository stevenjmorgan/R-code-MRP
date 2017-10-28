### Christian Right Influence and Policy Representation in the US States 
### CR Influence scores from Kim Conger, Univ. of Cincinnati

### Pre-Process Data

# Read in data from one-column .csv file (stored locally)
# 2008 CR Influence Scores for 50 states
CR_2008 <- read.csv("CI_Scores_2014.csv", stringsAsFactors = FALSE)

# Parse out data from one column into state variable and score variable for 2008
CR_2008$state_name <- sub(" .*", "", CR_2008$state)
CR_2008$score_2008 <- sub(".*? (.+)", "\\1", CR_2008$state)
CR_2008$score_2008 <- sub(" .*", "", CR_2008$score_2008)

# Remove first column w/ all data stored as type character
CR_2008 <- subset(CR_2008, select = -c(state) )
CR_2008$score_2008 <- as.numeric(CR_2008$score_2008)

# Ensure data is desired type
class(CR_2008$state_name)
class(CR_2008$score_2008)

# Add a vector for year of scores
CR_2008$year <- "2008"

# Reorder columns
CR_2008 <- CR_2008[c(1,3,2)]

# Rename CR influence score variable
names(CR_2008)[names(CR_2008) == 'score_2008'] <- 'CR_score'

# Rank scores
CR_2008$rank_year <- rank(-CR_2008$CR_score, ties.method = c("first"))

# Read in data from one-column .csv file (stored locally)
# 2000 and 2004 CR Influence scores from 50 states
CR_2000 <- read.csv("CR_Scores_2000_2004.csv", stringsAsFactors = FALSE)

# Parse data from one column; resulting vectors for state and 2000 and 2004
# CR influence scores
CR_2000$state_name <- sub(" .*", "", CR_2000$state)
CR_2000$score_2004 <- sub(".*? (.+)", "\\1", CR_2000$state)
CR_2000$score_2000 <- sub(".*? (.+)", "\\1", CR_2000$score_2004)
CR_2000$score_2004 <- sub(" .*", "", CR_2000$score_2004)

# Remove first column w/ all data stored as type character
CR_2000 <- subset(CR_2000, select = -c(state))
CR_2000$score_2000 <- as.numeric(CR_2000$score_2000)
CR_2000$score_2004 <- as.numeric(CR_2000$score_2004)

# Create dataframe for 2004 CR Influence scores
CR_2004 <- data.frame(CR_2000$state_name, CR_2000$score_2004)
colnames(CR_2004) <- c("state_name", "score_2004")
CR_2004$year <- "2004"
CR_2004$state_name <- as.character(CR_2004$state_name)

# Remove 2004 scores from 2000 dataset and add year to data
CR_2000 <- subset(CR_2000, select = -c(score_2004))
CR_2000$year <- "2000"

# Reorder and rename columns
CR_2000 <- CR_2000[c(1,3,2)]
CR_2004 <- CR_2004[c(1,3,2)]
names(CR_2000)[names(CR_2000) == 'score_2000'] <- 'CR_score'
names(CR_2004)[names(CR_2004) == 'score_2004'] <- 'CR_score'

# Rank scores
CR_2000$rank_year <- rank(-CR_2000$CR_score, ties.method = c("first"))
CR_2004$rank_year <- rank(-CR_2004$CR_score, ties.method = c("first"))

# Ensure data is desired type
class(CR_2000$state_name)
class(CR_2000$score_2000)
class(CR_2004$state_name)
class(CR_2004$score_2004)

# Merge dataframes into long-format; unit of analysis = state-year
CR_dataset <- rbind(CR_2000, CR_2004, CR_2008)

# Merge dataframes into wide-format; unit of analysis = state
wide_CR <- merge(CR_2000, CR_2004, by.x = "state_name", by.y = "state_name")
wide_CR <- merge(wide_CR, CR_2008, by.x = "state_name", by.y = "state_name")
names(wide_CR)[names(wide_CR) == '']
colnames(wide_CR) <- c("State","delete","CR_2000", "Rank_2000", "delete2", 
                       "CR_2004", "Rank_2004", "delete3","CR_2008", "Rank_2008")
wide_CR <- subset(wide_CR, select = -c(delete, delete2, delete3))
wide_CR$CR_2008 <- round(wide_CR$CR_2008, 2)

# install.packages("stargazer")
library(stargazer)
stargazer(wide_CR, summary = F)

# Sort dataframe by year and then state
CR_dataset <- CR_dataset[order(CR_dataset$year, CR_dataset$state_name),]
row.names(CR_dataset) <- NULL

# Ensure merged dataframe has 150 rows and 4 columns
dim(CR_dataset)


### Pre-Process State Policy Data

### Read in The Correlates of State Policy Project dataset from the INSTITUTE 
### FOR PUBLIC POLICY AND SOCIAL RESEARCH (IPPSR) at Michigan State University
### http://ippsr.msu.edu/public-policy/correlates-state-policy
### (saved locally for the purposes of this script)
policy <- read.csv("correlatesofstatepolicyproject.csv", stringsAsFactors = 
                       FALSE)

# Subset data for policy from 2000-2016 and appropriate variable
policy_2000 <- policy[which(policy$year >= 2000 & policy$year <= 2014),]
policy_data <- subset(policy_2000, select = c("year", "st", 
    "abortion_consent_1992_2014", "abortion_consent_1992_2014", 
    "abortion_partial_birth", "abortion_medicaid", "w_ec_access",
    "w_education_biblereading", "w_gayrights_public_accomodations",
    "w_gayrights_civilunions_marriage", "gayrights_hatecrimes", 
    "gayrights_ban_sodomy", "gaymarban", "soexorder", "solaw", "govparty_a",
    "leg_cont", "divided_gov", "per_leg_of_govs_pty", "dem_unified",
    "rep_unified", "hou_chamber", "sen_chamber", "hou_majority", "sen_dem", 
    "sen_dem", "sen_majority", "h_diffs", "s_diffs", "h_diffs", "h_dem_sd",
    "s_dem_sd", "s_dem_sd", "s_rep_sd", "h_chamber_sd", "s_chamber_sd", 
    "leg_realsalary", "bowen_legprof_firstdim", "pctfemaleleg", "citi6013",
    "wideo", "ideo", "mood", "democrat", "republican", "liberal", 
    "conservative"))


# Rename state variable
names(policy_data)[names(policy_data) == 'st'] <- 'state_name'

rm(policy, policy_2000)

policy_table <- cbind(policy_data$year, policy_data$state_name, 
    policy_data$abortion_consent_1992_2014, policy_data$abortion_medicaid,
    policy_data$w_gayrights_civilunions_marriage, policy_data$w_gayrights_public_accomodations,
    policy_data$gayrights_hatecrimes, policy_data$gayrights_ban_sodomy,
    policy_data$gaymarban)

### Merge CR Influence Scores dataframe with State Policy dataframe
CR_policy <- merge(CR_dataset, policy_data, by.x = c("state_name", "year"))
CR_policy <- CR_policy[order(CR_policy$year, CR_policy$state_name),]
row.names(CR_policy) <- NULL

# Policy descriptives
library(Hmisc)
describe(CR_policy$abortion_consent_1992_2014[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
# install.packages("moments")
library(moments)
skewness(CR_policy$abortion_consent_1992_2014[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
describe(CR_policy$abortion_medicaid[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
skewness(CR_policy$abortion_medicaid[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
describe(CR_policy$w_gayrights_civilunions_marriage[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
skewness(CR_policy$w_gayrights_civilunions_marriage[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
describe(CR_policy$w_gayrights_public_accomodations[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
skewness(CR_policy$w_gayrights_public_accomodations[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
describe(CR_policy$gayrights_ban_sodomy[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
skewness(CR_policy$gayrights_ban_sodomy[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
describe(CR_policy$gayrights_hatecrimes[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])
skewness(CR_policy$gayrights_hatecrimes[CR_policy$year == "2000" |CR_policy$year == "2004" |CR_policy$year == "2008"])

### Estimate state-level public opinion on abortion
# Read in data from nationally-representative surveys
# install.packages("Hmisc")
# install.packages("memisc")
library(foreign)
library(memisc)
library(car)

# Create variables for demographic variables of interest
pew08 <- read.spss("Pew08_abortion.por", to.data.frame = TRUE, trim.factor.names
                   = TRUE, use.value.labels = T)
pew08$female <- as.numeric(recode(pew08$SEX, "'Male' = 0; 'Female' = 1"))
dk <- "Don't know/Refused (VOL.)"
pew08$wbh <- recode(pew08$RACE_1, "'White' = 1; 'Black or African-American' = 2;
                    dk = 'NA'; 'Or some other race' = 'NA'; 
                    'Asian or Asian-American' = 'NA'")
pew08$wbh <- as.numeric(ifelse(pew08$HISP1 == 'Yes', 3, pew08$wbh))
pew08$age <- as.numeric(recode(pew08$AGE, "18:29 = 1; 30:44 = 2; 45:64 = 3; 
                               65:99 = 4"))
pew08$education <- as.numeric(recode(as.numeric(as.factor(pew08$EDUC)), 
    "1:2 = 1; 3:4 = 2; 5 = 3; 6 = 4; 7 = 5; 9 = 'NA'"))
pew08$region_num <- as.numeric(recode(pew08$SCREGION, "'Northeast' = 1; 'Midwest' 
    = 2; 'South' = 3; 'West' = 4"))
pew08$region <- recode(pew08$SCREGION, "'Northeast' = 'northeast'; 'Midwest' 
    = 'midwest'; 'South' = 'south'; 'West' = 'west'")
pew08$region <- ifelse(pew08$SSTATE == "D.C.", "dc", as.character(pew08$region))
pew08$state_num <- as.numeric(recode(pew08$SSTATE, "'Alaska' = 1; 'Alabama' = 2; 
    'Arkansas' = 3; ' Arizona' = 4; 'California' = 5; 'Colorado' = 6; 
    'Connecticut' = 7; 'District of Columbia' = 8; 'Delaware' = 9; 
    'Florida' = 10; 'Georgia' = 11; 'Hawaii' = 12; 'Iowa' = 13; ' Idaho' = 14; 
    'Illinois' = 15; 'Indiana' = 16; 'Kansas' = 17; 'Kentucky' = 18; 'Louisiana'
    = 19; 'Massachussetts' = 20; 'Maryland' = 21; 'Maine' = 22; 'Michigan' = 23;
    'Minnesota' = 24; 'Missouri' = 25; 'Mississippi' = 26; 'Montana' = 27; 
    'North Carolina' = 28; 'North Dakota' = 29; 'Nebraska' = 30; 'New Hampshire'
    = 31; 'New Jersey' = 32; 'New Mexico' = 33; 'Nevada' = 34; 'New York' = 35; 
    'Ohio' = 36; 'Oklahoma' = 37; 'Oregon' = 38; 'Pennsylvania' = 39; 
    'Rhode Island' = 40; 'South Carolina' = 41; 'South Dakota' = 42; 'Tennessee'
    = 43; 'Texas' = 44; 'Utah' = 45; 'Virginia' = 46; 'Vermont' = 47; 
    'Washington' = 48; 'Wisconsin' = 49; 'West Virginia' = 50; 'Wyoming' = 51"))
pew08$state <- recode(pew08$SSTATE, "'Alaska' = 'AK'; 'Alabama' = 'AL'; 
    'Arkansas' = 'AR'; 'Arizona' = 'AZ'; 'California' = 'CA'; 'Colorado' = 'CO'; 
    'Connecticut' = 'CT'; 'District of Columbia' = 'DC'; 'Delaware' = 'DE'; 
    'Florida' = 'FL'; 'Georgia' = 'GA'; 'Hawaii' = 'HI'; 'Iowa' = 'IA'; 'Idaho'
    = 'ID'; 'Illinois' = 'IL'; 'Indiana' = 'IN'; 'Kansas' = 'KS'; 'Kentucky' = 
    'KY'; 'Louisiana' = 'LA'; 'Massachusetts' = 'MA'; 'Maryland' = 'MD'; 
    'Maine' = 'ME'; 'Michigan' = 'MI'; 'Minnesota' = 'MN'; 'Missouri' = 'MO'; 
    'Mississippi' = 'MS'; 'Montana' = 'MT'; 'North Carolina' = 'NC'; 
    'North Dakota' = 'ND'; 'Nebraska' = 'NE'; 'New Hampshire' = 'NH'; 
    'New Jersey' = 'NJ'; 'New Mexico' = 'NM'; 'Nevada' = 'NV'; 'New York' = 
    'NY'; 'Ohio' = 'OH'; 'Oklahoma' = 'OK'; 'Oregon' = 'OR'; 'Pennsylvania' = 
    'PA'; 'Rhode Island' = 'RI'; 'South Carolina' = 'SC'; 'South Dakota' = 'SD';
    'Tennessee' = 'TN'; 'Texas' = 'TX'; 'Utah' = 'UT'; 'Virginia' = 'VA'; 
    'Vermont' = 'VT'; 'Washington' = 'WA'; 'Wisconsin' = 'WI'; 'West Virginia' =
    'WV'; 'Wyoming' = 'WY'; 'D.C.' = 'DC'")

# 1 = Legal in all or most cases (Pro-Choice opinion)
pew08$yes_of_all <- recode(pew08$Q17, "'Legal in all cases' = 1; 
    'Legal in most cases' = 1; 'Illegal in most cases' = 0; 
    'Illegal in all cases' = 0; 'Don' = 0")
pew08$yes_of_ans <- recode(pew08$Q17, "'Legal in all cases' = 1; 
    'Legal in most cases' = 1; 'Illegal in most cases' = 0; 
    'Illegal in all cases' = 0; 'Don' = 'NA'")

# Read in state-level dataset to model state-level effects
Statelevel <- read.dta("state_level_update.dta", convert.underscore = TRUE)
Statelevel <- Statelevel[order(Statelevel$sstate.initnum),]

# Read in Census 2000 data for poststratification
census2000 <- read.dta("census2000.dta", convert.underscore = TRUE)

# Create index variables for individual-level model and poststratification
pew08$race_female <- (pew08$female * 3) + pew08$wbh
pew08$age_edu_cat <- 4 * (pew08$age - 1) + pew08$education
pew08$p_evang_full <- Statelevel$p.evang[pew08$state]
pew08$p_mormon_full <- Statelevel$p.mormon[pew08$state]
pew08$p_relig_full <- pew08$p_evang_full + pew08$p_mormon_full
pew08$p_kerry_full <- Statelevel$kerry.04[pew08$state]

# Create index variables for Census-level model
census2000$cstate.initnum <- match(census2000$cstate, Statelevel$sstate)
census2000$crace_female <- (census2000$cfemale * 3) + census2000$crace.WBH
census2000$cage_edu_cat <- 4 * (census2000$cage.cat - 1) + census2000$cedu.cat
census2000$cp_evang_full <- Statelevel$p.evang[census2000$cstate.initnum]
census2000$cp_mormon_full <- Statelevel$p.mormon[census2000$cstate.initnum]
census2000$cp_relig_full <- census2000$cp_evang_full + census2000$cp_mormon_full
census2000$cp_kerry_full <- Statelevel$kerry.04[census2000$cstate.initnum]

# Fit individual-level model
# Poll parameter not estimated since model is estimating data from one poll
# install.packages("arm")
library(arm)
individual_model_abortion <- glmer(formula = yes_of_all ~ (1|race_female) + 
    (1|age) + (1|education) + (1|age_edu_cat) + (1|state) + (1|region) + 
    p_relig_full + p_kerry_full, data = pew08, 
    family = binomial(link = "logit"))

# Model outputs probability that any adult will be pro-choice given the person's
# sex, race, age, education, and state
summary(individual_model_abortion)

# Coefficients and standard errors of estimated random effect for race-female
# categories
ranef(individual_model_abortion)$race_female
se.ranef(individual_model_abortion)$race_female

# No respondents from Hawaii or Alaska
# Create vector of state random effects for Hawaii and Alaska (set equal to 0)
summary(pew08$SSTATE == "Hawaii")
summary(pew08$SSTATE == "Alaska")
state.ranefs <- array(NA, c(51,1))
dimnames(state.ranefs) <- list(c(Statelevel$sstate), "effect")
for (i in Statelevel$sstate) {
    state.ranefs[i,1] <- ranef(individual_model_abortion)$state[i,1]
}
state.ranefs[,1][is.na(state.ranefs[,1])] <- 0

# Compute weighted averages of pro-choice probability to estimate the proportion
# of pro-choice supporters in each state: for any specific cell j, specifying a 
# set of individual demographic and geographic values, the results of the 
# opinion model above allow us to make a prediction of pro-gay support,theta-j.
# 'cellpred' calculates weighted averages and then makes a prediction for each 
# demographic-state type.
cellpred <- invlogit(fixef(individual_model_abortion)["(Intercept)"]
    + ranef(individual_model_abortion)$race_female[census2000$crace_female,1]
    + ranef(individual_model_abortion)$age[census2000$cage.cat,1]
    + ranef(individual_model_abortion)$education[census2000$cedu.cat,1]
    + ranef(individual_model_abortion)$age_edu_cat[census2000$cage_edu_cat,1]
    + state.ranefs[census2000$cstate,1]
    + ranef(individual_model_abortion)$region[census2000$cregion,1]
    + (fixef(individual_model_abortion)["p_relig_full"] * 
    census2000$cp_relig_full) + 
    (fixef(individual_model_abortion)["p_kerry_full"]*census2000$cp_kerry_full))

# Weight the prediction by the frequency of each cell and calculate the percent 
# within each state (weighted average of responses)
cellpredweighted <- cellpred * census2000$cpercent.state 
statepred <- 100 * as.vector(tapply(cellpredweighted, census2000$cstate, sum))
statepred

# Bind together vector of state predictions and merge with Christian Right and 
# policy dataframe
pred_df <- data.frame(cbind(Statelevel$sstate, as.numeric(statepred)))
names(pred_df)[names(pred_df) == "X1"] <- "state_name"
names(pred_df)[names(pred_df) == "X2"] <- "abortion_opinion"
pred_df$year <- "2008"
CR_pol_opin <- merge(CR_policy[CR_policy$year == "2008",], pred_df[pred_df$state_name != "DC",], by.x = c("state_name", "year"))
CR_pol_opin$abortion_opinion <- as.numeric(CR_pol_opin$abortion_opinion)

### Model abortion policy as a function of CR influence and public opinion
abort_fit_08 <- glm(abortion_consent_1992_2014 ~ CR_score + abortion_opinion, data = CR_pol_opin, family = "binomial")
summary(abort_fit_08)

### Estimate state-level opinion on abortion 2004
abc04 <- read.spss("abcwp955.por", to.data.frame = TRUE, trim.factor.names
                   = TRUE, use.value.labels = T)

# Create variables for demographic variables of interest
abc04$female <- as.numeric(recode(abc04$Q921, "'Male' = 0; 'Female' = 1"))
abc04$wbh <- as.numeric(recode(abc04$Q918, "'White' = 1; 'Black' = 2;
                    'Black Hispanic' = 2; 'White Hispanic' = 3; dk = 'NA'; 
                    'Other Race' = 'NA'; 'Hispanic (no race given)' = 3;
                    'Asian or Asian-American' = 'NA'"))
abc04$age <- as.numeric(recode(abc04$Q910, "18:29 = 1; 30:44 = 2; 45:64 = 3; 
                               65:99 = 4"))
abc04$education <- as.numeric(recode(abc04$Q909, "'8th grade or less' = 1; 
    'Some high school' = 1; 'Graduated high school' = 2; 'Some college' = 3; 
    'Graduated College' = 4; 'Post-graduate' = 4; 'DK/No opinion' = 'NA'"))
abc04$region_num <- as.numeric(recode(abc04$CENSDIV, "'Middle Atlantic' = 1; 
    'New England' = 1; 'East North Central' = 2; 'West North Central' = 2; 
    'Mountain' = 4; 'South Atlantic' = 3; 'East South Central' = 3; 
    'Pacific' = 4; 'West South Central' = 4"))
abc04$region <- recode(abc04$CENSDIV, "'Middle Atlantic' = 'northeast'; 
    'New England' = 'northeast'; 'East North Central' = 'midwest'; 
    'West North Central' = 'midwest'; 'South Atlantic' = 'south'; 
    'East South Central' = 'south'; 'West' = 'west'; 'Pacific' = 'west'; 
    'Mountain' = 'west'; 'West South Central' = 'west'")
#abc04$region <- ifelse(abc04$SSTATE == "D.C.", "dc", as.character(abc04$CENSDIV))
abc04$state_num <- as.numeric(recode(abc04$ABCNUM, "'Alaska' = 1; 'Alabama' = 2; 
    'Arkansas' = 3; ' Arizona' = 4; 'California' = 5; 'Colorado' = 6; 
    'Connecticut' = 7; 'District of Columbia' = 8; 'Delaware' = 9; 
    'Florida' = 10; 'Georgia' = 11; 'Hawaii' = 12; 'Iowa' = 13; ' Idaho' = 14; 
    'Illinois' = 15; 'Indiana' = 16; 'Kansas' = 17; 'Kentucky' = 18; 'Louisiana'
     = 19; 'Massachussetts' = 20; 'Maryland' = 21; 'Maine' = 22; 'Michigan' = 23;
    'Minnesota' = 24; 'Missouri' = 25; 'Mississippi' = 26; 'Montana' = 27; 
    'North Carolina' = 28; 'North Dakota' = 29; 'Nebraska' = 30; 'New Hampshire'
    = 31; 'New Jersey' = 32; 'New Mexico' = 33; 'Nevada' = 34; 'New York' = 35; 
    'Ohio' = 36; 'Oklahoma' = 37; 'Oregon' = 38; 'Pennsylvania' = 39; 
    'Rhode Island' = 40; 'South Carolina' = 41; 'South Dakota' = 42; 'Tennessee'
    = 43; 'Texas' = 44; 'Utah' = 45; 'Virginia' = 46; 'Vermont' = 47; 
    'Washington' = 48; 'Wisconsin' = 49; 'West Virginia' = 50; 'Wyoming' = 51"))
abc04$state <- recode(abc04$ABCNUM, "'Alaska' = 'AK'; 'Alabama' = 'AL'; 
    'Arkansas' = 'AR'; 'Arizona' = 'AZ'; 'California' = 'CA'; 'Colorado' = 'CO'; 
    'Connecticut' = 'CT'; 'District of Columbia' = 'DC'; 'Delaware' = 'DE'; 
    'Florida' = 'FL'; 'Georgia' = 'GA'; 'Hawaii' = 'HI'; 'Iowa' = 'IA'; 'Idaho'
    = 'ID'; 'Illinois' = 'IL'; 'Indiana' = 'IN'; 'Kansas' = 'KS'; 'Kentucky' = 
    'KY'; 'Louisiana' = 'LA'; 'Massachusetts' = 'MA'; 'Maryland' = 'MD'; 
    'Maine' = 'ME'; 'Michigan' = 'MI'; 'Minnesota' = 'MN'; 'Missouri' = 'MO'; 
    'Mississippi' = 'MS'; 'Montana' = 'MT'; 'North Carolina' = 'NC'; 
    'North Dakota' = 'ND'; 'Nebraska' = 'NE'; 'New Hampshire' = 'NH'; 
    'New Jersey' = 'NJ'; 'New Mexico' = 'NM'; 'Nevada' = 'NV'; 'New York' = 
    'NY'; 'Ohio' = 'OH'; 'Oklahoma' = 'OK'; 'Oregon' = 'OR'; 'Pennsylvania' = 
    'PA'; 'Rhode Island' = 'RI'; 'South Carolina' = 'SC'; 'South Dakota' = 'SD';
    'Tennessee' = 'TN'; 'Texas' = 'TX'; 'Utah' = 'UT'; 'Virginia' = 'VA'; 
    'Vermont' = 'VT'; 'Washington' = 'WA'; 'Wisconsin' = 'WI'; 'West Virginia' =
    'WV'; 'Wyoming' = 'WY'; 'D.C.' = 'DC'")

# 1 = Legal in all or most cases (Pro-Choice opinion)
abc04$yes_of_all <- recode(abc04$Q32, "'Legal in all cases' = 1; 
    'Legal in most cases' = 1; 'Illegal in most cases' = 0; 
    'Illegal in all cases' = 0; 'DK/No opinion' = 0; else = 0")
abc04$yes_of_ans <- recode(abc04$Q32, "'Legal in all cases' = 1; 
    'Legal in most cases' = 1; 'Illegal in most cases' = 0; 
    'Illegal in all cases' = 0; 'DK/No opinion' = 'NA'; else = 'NA'")

# Create index variables for individual-level model and poststratification
abc04$race_female <- (abc04$female * 3) + abc04$wbh
abc04$age_edu_cat <- 4 * (abc04$age - 1) + abc04$education
abc04$p_evang_full <- Statelevel$p.evang[abc04$state]
abc04$p_mormon_full <- Statelevel$p.mormon[abc04$state]
abc04$p_relig_full <- abc04$p_evang_full + abc04$p_mormon_full
abc04$p_kerry_full <- Statelevel$kerry.04[abc04$state]

# Fit individual-level model
# Poll parameter not estimated since model is estimating data from one poll
individual_model_abortion04 <- glmer(formula = yes_of_all ~ (1|race_female) + 
    (1|age) + (1|education) + (1|age_edu_cat) + (1|state) + (1|region) + 
    p_relig_full + p_kerry_full, data = abc04, 
    family = binomial(link = "logit"))

# Model outputs probability that any adult will be pro-choice given the person's
# sex, race, age, education, and state
summary(individual_model_abortion04)

# Coefficients and standard errors of estimated random effect for race-female
# categories
ranef(individual_model_abortion04)$race_female
se.ranef(individual_model_abortion04)$race_female

# No respondents from Hawaii or Alaska
# Create vector of state random effects for Hawaii and Alaska (set equal to 0)
summary(abc04$ABCNUM == "Hawaii")
summary(abc04$ABCNUM == "Alaska")
state.ranefs04 <- array(NA, c(51,1))
dimnames(state.ranefs04) <- list(c(Statelevel$sstate), "effect")
for (i in Statelevel$sstate) {
    state.ranefs04[i,1] <- ranef(individual_model_abortion04)$state[i,1]
}
state.ranefs04[,1][is.na(state.ranefs04[,1])] <- 0

# Compute weighted averages of pro-choice probability to estimate the proportion
# of pro-choice supporters in each state: for any specific cell j, specifying a 
# set of individual demographic and geographic values, the results of the 
# opinion model above allow us to make a prediction of pro-gay support,theta-j.
# 'cellpred' calculates weighted averages and then makes a prediction for each 
# demographic-state type.
cellpred04 <- invlogit(fixef(individual_model_abortion04)["(Intercept)"]
     + ranef(individual_model_abortion04)$race_female[census2000$crace_female,1]
     + ranef(individual_model_abortion04)$age[census2000$cage.cat,1]
     + ranef(individual_model_abortion04)$education[census2000$cedu.cat,1]
     + ranef(individual_model_abortion04)$age_edu_cat[census2000$cage_edu_cat,1]
     + state.ranefs04[census2000$cstate,1]
     + ranef(individual_model_abortion04)$region[census2000$cregion,1]
     + (fixef(individual_model_abortion04)["p_relig_full"] * 
    census2000$cp_relig_full) + 
     (fixef(individual_model_abortion04)["p_kerry_full"]*census2000$cp_kerry_full))

# Weight the prediction by the frequency of each cell and calculate the percent 
# within each state (weighted average of responses)
cellpredweighted04 <- cellpred04 * census2000$cpercent.state 
statepred04 <- 100 * as.vector(tapply(cellpredweighted04, census2000$cstate, sum))
statepred04

# Bind together vector of state predictions and merge with Christian Right and 
# policy dataframe
pred_df04 <- data.frame(cbind(Statelevel$sstate, as.numeric(statepred04)))
names(pred_df04)[names(pred_df04) == "X1"] <- "state_name"
names(pred_df04)[names(pred_df04) == "X2"] <- "abortion_opinion"
pred_df04$year <- "2004"
pred_df0408 <- data.frame(rbind(pred_df, pred_df04))
CR_pol_opin04 <- merge(CR_policy[CR_policy$year == "2004",], pred_df04[pred_df04$state_name != "DC",], by.x = c("state_name", "year"))
CR_pol_opin04$abortion_opinion <- as.numeric(CR_pol_opin04$abortion_opinion)

### Model abortion policy as a function of CR influence and public opinion 2004
abort_fit_04 <- glm(abortion_consent_1992_2014 ~ CR_score + abortion_opinion, 
                    data = CR_pol_opin04, family = "binomial")
summary(abort_fit_04)


### Estimate state-level opinion on abortion 2000
abc00 <- read.spss("abc15056.por", to.data.frame = TRUE, trim.factor.names
                   = TRUE, use.value.labels = T)

# Create variables for demographic variables of interest
abc00$female <- as.numeric(recode(abc00$SEX, "'Male' = 0; 'Female' = 1"))
abc00$wbh <- as.numeric(recode(abc00$RACE, "'White' = 1; 'Black' = 2;
                               'Black Hispanic' = 2; 'White Hispanic' = 3; dk = 'NA'; 
                               'Other Race' = 'NA'; 'Hispanic(no race given)' = 3;
                               'Asian or Asian-American' = 'NA'; else = 'NA'"))
abc00$age <- as.numeric(recode(abc00$AGE, "18:29 = 1; 30:44 = 2; 45:64 = 3; 
                               65:99 = 4"))
abc00$education <- as.numeric(recode(abc00$EDUC, "'8th grade or less' = 1; 
    'Some high school' = 1; 'Graduated high school' = 2; 'Some college' = 3; 
    'Graduated College' = 4; 'Post-graduate' = 4; 'DK/No opinion' = 'NA'; 
    else = 'NA'"))
east <- 'Censdiv = 1,2 (East)'
midwest <- 'Censdiv = 3,4 (Midwest)'
south <- 'Censdiv = 5,6,7 (South)'
west <- 'Censdiv = 8,9 (West)'
abc00$region <- recode(abc00$REGION, "east = 'northeast'; 
    midwest = 'midwest'; south = 'south'; west = 'west'")
abc00$state_num <- as.numeric(recode(abc00$STATE, "'Alaska' = 1; 'Alabama' = 2; 
    'Arkansas' = 3; ' Arizona' = 4; 'California' = 5; 'Colorado' = 6; 
    'Connecticut' = 7; 'District of Columbia' = 8; 'Delaware' = 9; 
    'Florida' = 10; 'Georgia' = 11; 'Hawaii' = 12; 'Iowa' = 13; ' Idaho' = 14; 
    'Illinois' = 15; 'Indiana' = 16; 'Kansas' = 17; 'Kentucky' = 18; 'Louisiana'
    = 19; 'Massachussetts' = 20; 'Maryland' = 21; 'Maine' = 22; 'Michigan' = 23;
    'Minnesota' = 24; 'Missouri' = 25; 'Mississippi' = 26; 'Montana' = 27; 
    'North Carolina' = 28; 'North Dakota' = 29; 'Nebraska' = 30; 'New Hampshire'
    = 31; 'New Jersey' = 32; 'New Mexico' = 33; 'Nevada' = 34; 'New York' = 35; 
    'Ohio' = 36; 'Oklahoma' = 37; 'Oregon' = 38; 'Pennsylvania' = 39; 
    'Rhode Island' = 40; 'South Carolina' = 41; 'South Dakota' = 42; 'Tennessee'
    = 43; 'Texas' = 44; 'Utah' = 45; 'Virginia' = 46; 'Vermont' = 47; 
    'Washington' = 48; 'Wisconsin' = 49; 'West Virginia' = 50; 'Wyoming' = 51"))
abc00$state <- recode(abc00$STATE, "'Alaska' = 'AK'; 'Alabama' = 'AL'; 
    'Arkansas' = 'AR'; 'Arizona' = 'AZ'; 'California' = 'CA'; 'Colorado' = 'CO'; 
    'Connecticut' = 'CT'; 'District of Columbia' = 'DC'; 'Delaware' = 'DE'; 
    'Florida' = 'FL'; 'Georgia' = 'GA'; 'Hawaii' = 'HI'; 'Iowa' = 'IA'; 'Idaho'
    = 'ID'; 'Illinois' = 'IL'; 'Indiana' = 'IN'; 'Kansas' = 'KS'; 'Kentucky' = 
    'KY'; 'Louisiana' = 'LA'; 'Massachusetts' = 'MA'; 'Maryland' = 'MD'; 
    'Maine' = 'ME'; 'Michigan' = 'MI'; 'Minnesota' = 'MN'; 'Missouri' = 'MO'; 
    'Mississippi' = 'MS'; 'Montana' = 'MT'; 'North Carolina' = 'NC'; 
    'North Dakota' = 'ND'; 'Nebraska' = 'NE'; 'New Hampshire' = 'NH'; 
    'New Jersey' = 'NJ'; 'New Mexico' = 'NM'; 'Nevada' = 'NV'; 'New York' = 
    'NY'; 'Ohio' = 'OH'; 'Oklahoma' = 'OK'; 'Oregon' = 'OR'; 'Pennsylvania' = 
    'PA'; 'Rhode Island' = 'RI'; 'South Carolina' = 'SC'; 'South Dakota' = 'SD';
    'Tennessee' = 'TN'; 'Texas' = 'TX'; 'Utah' = 'UT'; 'Virginia' = 'VA'; 
    'Vermont' = 'VT'; 'Washington' = 'WA'; 'Wisconsin' = 'WI'; 'West Virginia' =
    'WV'; 'Wyoming' = 'WY'; 'D.C.' = 'DC'")

# 1 = Legal in all or most cases (Pro-Choice opinion)
abc00$yes_of_all <- recode(abc00$Q18, "'Legal, All Cases' = 1; 
    'Legal, Most Case' = 1; 'ILLEGAL, Most Cases' = 0; 
    'ILLEGAL, All Cases' = 0; else = 0")
abc00$yes_of_ans <- recode(abc00$Q18, "'Legal, All Cases' = 1; 
    'Legal, Most Case' = 1; 'ILLEGAL, Most Cases' = 0; 
    'ILLEGAL, All Cases' = 0; else = 'NA'")

# Create index variables for individual-level model and poststratification
abc00$race_female <- (abc00$female * 3) + abc00$wbh
abc00$age_edu_cat <- 4 * (abc00$age - 1) + abc00$education
abc00$p_evang_full <- Statelevel$p.evang[abc00$state]
abc00$p_mormon_full <- Statelevel$p.mormon[abc00$state]
abc00$p_relig_full <- abc00$p_evang_full + abc00$p_mormon_full
abc00$p_kerry_full <- Statelevel$kerry.04[abc00$state]

# Fit individual-level model
# Poll parameter not estimated since model is estimating data from one poll
individual_model_abortion00 <- glmer(formula = yes_of_all ~ (1|race_female) + 
    (1|age) + (1|education) + (1|age_edu_cat) + (1|state) + (1|region) + 
    p_relig_full + p_kerry_full, data = abc00, 
    family = binomial(link = "logit"))

# Model outputs probability that any adult will be pro-choice given the person's
# sex, race, age, education, and state
summary(individual_model_abortion00)

# Coefficients and standard errors of estimated random effect for race-female
# categories
ranef(individual_model_abortion00)$race_female
se.ranef(individual_model_abortion00)$race_female

# No respondents from Hawaii or Alaska
# Create vector of state random effects for Hawaii and Alaska (set equal to 0)
summary(abc00$ABCNUM == "Hawaii")
summary(abc00$ABCNUM == "Alaska")
state.ranefs00 <- array(NA, c(51,1))
dimnames(state.ranefs00) <- list(c(Statelevel$sstate), "effect")
for (i in Statelevel$sstate) {
    state.ranefs00[i,1] <- ranef(individual_model_abortion00)$state[i,1]
}
state.ranefs00[,1][is.na(state.ranefs00[,1])] <- 0

# Compute weighted averages of pro-choice probability to estimate the proportion
# of pro-choice supporters in each state: for any specific cell j, specifying a 
# set of individual demographic and geographic values, the results of the 
# opinion model above allow us to make a prediction of pro-gay support,theta-j.
# 'cellpred' calculates weighted averages and then makes a prediction for each 
# demographic-state type.
cellpred00 <- invlogit(fixef(individual_model_abortion00)["(Intercept)"]
     + ranef(individual_model_abortion00)$race_female[census2000$crace_female,1]
     + ranef(individual_model_abortion00)$age[census2000$cage.cat,1]
     + ranef(individual_model_abortion00)$education[census2000$cedu.cat,1]
     + ranef(individual_model_abortion00)$age_edu_cat[census2000$cage_edu_cat,1]
     + state.ranefs00[census2000$cstate,1]
     + ranef(individual_model_abortion00)$region[census2000$cregion,1]
     + (fixef(individual_model_abortion00)["p_relig_full"] * 
     census2000$cp_relig_full) + 
     fixef(individual_model_abortion00)["p_kerry_full"]*census2000$cp_kerry_full)

# Weight the prediction by the frequency of each cell and calculate the percent 
# within each state (weighted average of responses)
cellpredweighted00 <- cellpred00 * census2000$cpercent.state 
statepred00 <- 100 * as.vector(tapply(cellpredweighted00, census2000$cstate, 
                                      sum))
statepred00

# Bind together vector of state predictions and merge with Christian Right and 
# policy dataframe
pred_df00 <- data.frame(cbind(Statelevel$sstate, as.numeric(statepred00)))
names(pred_df00)[names(pred_df00) == "X1"] <- "state_name"
names(pred_df00)[names(pred_df00) == "X2"] <- "abortion_opinion"
pred_df00$year <- "2000"
pred_df000408 <- data.frame(rbind(pred_df0408, pred_df00))
CR_pol_opin00 <- merge(CR_policy[CR_policy$year == "2000",], 
    pred_df00[pred_df00$state_name != "DC",], by.x = c("state_name", "year"))
CR_pol_opin00$abortion_opinion <- as.numeric(CR_pol_opin00$abortion_opinion)

### Model abortion policy as a function of CR influence and public opinion 2004
abort_fit_00 <- glm(abortion_consent_1992_2014 ~ CR_score + abortion_opinion, 
                    data = CR_pol_opin00, family = "binomial")
summary(abort_fit_00)

# Merge dataframe with policy in place and state opinion for 2000, 2004,and 2008
CR_pol_opin000408 <- merge(CR_policy[CR_policy$year == "2000"|CR_policy$year == 
    "2004"|CR_policy$year == "2008",], pred_df000408[pred_df000408$state_name 
    != "DC",], by.x = c("state_name", "year"))
#CR_pol_opin000408$abortion_opinion <- 
#    as.numeric(CR_pol_opin000408$abortion_opinion)

abort_pooled00 <- data.frame(CR_pol_opin000408$state_name, CR_pol_opin000408$year,
                           round(as.numeric(as.character(CR_pol_opin000408$abortion_opinion)), 2))
abort_pooled00 <- abort_pooled00[which(abort_pooled00$CR_pol_opin000408.year == "2000"),]
stargazer(abort_pooled00, summary = F)

# Model state policy of forced counseling before abortion procedure as a 
# function of CR influence, state opinion on abortion, and time
abortion_all_years <- glm(abortion_consent_1992_2014 ~ CR_score + 
    abortion_opinion + as.factor(year), data = CR_pol_opin000408, 
    family = "binomial")
summary(abortion_all_years)

# Model state policy of forced counseling before abortion procedure as a 
# function of CR influence, state opinion on abortion, and time
abortion_medi_all <- glm(abortion_medicaid ~ CR_score + 
    abortion_opinion + as.factor(year), data = CR_pol_opin000408, 
    family = "binomial")
summary(abortion_medi_all)

# Pearson's r between CR influence and public opinion
cor(CR_pol_opin000408$CR_score, CR_pol_opin000408$abortion_opinion)

# Predicted Probabilities Mandatory Counseling
sim_data <- data.frame(abortion_opinion = mean(CR_pol_opin000408$abortion_opinion), CR_score = seq(from=0,to=5,length.out=501), year = 2004)
OutHats <- predict(abortion_all_years, se.fit = TRUE, newdata = sim_data)
OutHatsUB <- OutHats$fit + (1.96*OutHats$se.fit)
OutHatsLB <- OutHats$fit - (1.96*OutHats$se.fit)
OutHats <- cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats <- data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

par(mfrow = c(1,2))
both <- cbind(sim_data,OutHats)
plot(both$CR_score, both$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="Christian Right Score",ylab="Predicted Pr(Mandatory Counseling)")
lines(both$CR_score, both$OutHatsUB,lty=2)
lines(both$CR_score, both$OutHatsLB,lty=2)

sim_data2 <- data.frame(CR_score = mean(CR_pol_opin000408$CR_score), 
    abortion_opinion = seq(from = 0, to = 100, length.out = 101), year = 2004)
OutHats2 <- predict(abortion_all_years, se.fit = TRUE, newdata = sim_data2)
OutHatsUB2 <- OutHats2$fit + (1.96*OutHats2$se.fit)
OutHatsLB2 <- OutHats2$fit - (1.96*OutHats2$se.fit)
OutHats2 <- cbind(as.data.frame(OutHats2), OutHatsUB2, OutHatsLB2)
OutHats2 <- data.frame(lapply(OutHats2,binomial(link = "logit")$linkinv))

both2 <- cbind(sim_data2, OutHats2)
plot(both2$abortion_opinion, both2$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="Abortion Opinion",ylab="Predicted Pr(Mandatory Counseling)")
lines(both2$abortion_opinion, both2$OutHatsUB2,lty=2)
lines(both2$abortion_opinion, both2$OutHatsLB2,lty=2)


# Odds Ratios
lreg.or <- function(model)
 {
  coeffs <- coef(summary(abortion_all_years))
  lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2])
  or <- exp(coeffs[ ,1])
  uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2])
  lreg.or <- cbind(lci, or, uci)
  lreg.or
  }

lreg.or(abortion_all_years)

# Predicted Probabilities Medicaid Coverage
sim_data <- data.frame(abortion_opinion = mean(CR_pol_opin000408$abortion_opinion), CR_score = seq(from=0,to=5,length.out=501), year = 2004)
OutHats <- predict(abortion_medi_all, se.fit = TRUE, newdata = sim_data)
OutHatsUB <- OutHats$fit + (1.96*OutHats$se.fit)
OutHatsLB <- OutHats$fit - (1.96*OutHats$se.fit)
OutHats <- cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats <- data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

both <- cbind(sim_data,OutHats)
plot(both$CR_score, both$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="Christian Right Score",ylab="Predicted Pr(Medicaid Coverage)")
lines(both$CR_score, both$OutHatsUB,lty=2)
lines(both$CR_score, both$OutHatsLB,lty=2)

sim_data2 <- data.frame(CR_score = mean(CR_pol_opin000408$CR_score), 
                        abortion_opinion = seq(from = 0, to = 100, length.out = 101), year = 2004)
OutHats2 <- predict(abortion_medi_all, se.fit = TRUE, newdata = sim_data2)
OutHatsUB2 <- OutHats2$fit + (1.96*OutHats2$se.fit)
OutHatsLB2 <- OutHats2$fit - (1.96*OutHats2$se.fit)
OutHats2 <- cbind(as.data.frame(OutHats2), OutHatsUB2, OutHatsLB2)
OutHats2 <- data.frame(lapply(OutHats2,binomial(link = "logit")$linkinv))

both2 <- cbind(sim_data2, OutHats2)
plot(both2$abortion_opinion, both2$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="Abortion Opinion",ylab="Predicted Pr(Medicaid Coverage)")
lines(both2$abortion_opinion, both2$OutHatsUB2,lty=2)
lines(both2$abortion_opinion, both2$OutHatsLB2,lty=2)


# Create tables with Stargazer
# install.packages("stargazer")
library(stargazer)
stargazer(abortion_all_years, abortion_medi_all)
stargazer(lreg.or(abortion_all_years))

# Read in PSRA/Newsweek poll for public opinion on gay marriage in 2000
g2004 <- read.spss("g200444.por", to.data.frame = TRUE, trim.factor.names
                    = TRUE, use.value.labels = T)
g2004$female <- as.numeric(recode(g2004$S2, "'male' = 0; 'female' = 1")) - 1
g2004$wbh <- as.numeric(recode(g2004$D4A, "'White' = 1; 'African-American' = 2; 
    'Hispanic' = 3; dk = 'NA'; 'Other or mixed race (SPECIFY)' = 'NA'; 
    'Asian' = 'NA'; 'Refused' = 'NA'; else = 'NA'"))
g2004$age <- as.numeric(recode(g2004$D2, "18:29 = 1; 30:44 = 2; 45:64 = 3; 
                               65:99 = 4"))
g2004$education <- as.numeric(recode(g2004$D3, 
    "'None up to Grade 4' = 1; 'Grades 5-7' = 1; 'Grade 8' = 1;
    'High school incomplete (Grade 9-11)' = 1; 
    'High School Graduate (Grade 12)' = 2; 
    'Technical/Trade/Business after high school' = 2; 
    'College/university incomplete' = 2; 
    'College/university graduate' = 3; 
    'DK/No opinion' = 'NA'; else = 'NA'"))
#g2004$education <- as.numeric(ifelse(g2004$D3A == "Yes, post-graduate education", 4, g2004$education))
g2004$region <- recode(g2004$CREGION, "'East' = 'northeast'; 'Midwest' 
                       = 'midwest'; 'South' = 'south'; 'West' = 'west'")
g2004$region <- ifelse(g2004$STATE == "D.C.", "dc", as.character(g2004$region))
g2004$state_num <- as.numeric(recode(g2004$STATE, "'Alaska' = 1; 'Alabama' = 2; 
    'Arkansas' = 3; ' Arizona' = 4; 'California' = 5; 'Colorado' = 6; 
    'Connecticut' = 7; 'District of Columbia' = 8; 'Delaware' = 9; 
    'Florida' = 10; 'Georgia' = 11; 'Hawaii' = 12; 'Iowa' = 13; ' Idaho' = 14; 
    'Illinois' = 15; 'Indiana' = 16; 'Kansas' = 17; 'Kentucky' = 18; 'Louisiana'
    = 19; 'Massachussetts' = 20; 'Maryland' = 21; 'Maine' = 22; 'Michigan' = 23;
    'Minnesota' = 24; 'Missouri' = 25; 'Mississippi' = 26; 'Montana' = 27; 
    'North Carolina' = 28; 'North Dakota' = 29; 'Nebraska' = 30; 'New Hampshire'
    = 31; 'New Jersey' = 32; 'New Mexico' = 33; 'Nevada' = 34; 'New York' = 35; 
    'Ohio' = 36; 'Oklahoma' = 37; 'Oregon' = 38; 'Pennsylvania' = 39; 
    'Rhode Island' = 40; 'South Carolina' = 41; 'South Dakota' = 42; 'Tennessee'
    = 43; 'Texas' = 44; 'Utah' = 45; 'Virginia' = 46; 'Vermont' = 47; 
    'Washington' = 48; 'Wisconsin' = 49; 'West Virginia' = 50; 'Wyoming' = 51"))
g2004$state <- recode(g2004$STATE, "'Alaska' = 'AK'; 'Alabama' = 'AL'; 
    'Arkansas' = 'AR'; 'Arizona' = 'AZ'; 'California' = 'CA'; 'Colorado' = 'CO'; 
    'Connecticut' = 'CT'; 'District of Columbia' = 'DC'; 'Delaware' = 'DE'; 
    'Florida' = 'FL'; 'Georgia' = 'GA'; 'Hawaii' = 'HI'; 'Iowa' = 'IA'; 'Idaho'
     = 'ID'; 'Illinois' = 'IL'; 'Indiana' = 'IN'; 'Kansas' = 'KS'; 'Kentucky' = 
    'KY'; 'Louisiana' = 'LA'; 'Massachusetts' = 'MA'; 'Maryland' = 'MD'; 
    'Maine' = 'ME'; 'Michigan' = 'MI'; 'Minnesota' = 'MN'; 'Missouri' = 'MO'; 
    'Mississippi' = 'MS'; 'Montana' = 'MT'; 'North Carolina' = 'NC'; 
    'North Dakota' = 'ND'; 'Nebraska' = 'NE'; 'New Hampshire' = 'NH'; 
    'New Jersey' = 'NJ'; 'New Mexico' = 'NM'; 'Nevada' = 'NV'; 'New York' = 
    'NY'; 'Ohio' = 'OH'; 'Oklahoma' = 'OK'; 'Oregon' = 'OR'; 'Pennsylvania' = 
    'PA'; 'Rhode Island' = 'RI'; 'South Carolina' = 'SC'; 'South Dakota' = 'SD';
    'Tennessee' = 'TN'; 'Texas' = 'TX'; 'Utah' = 'UT'; 'Virginia' = 'VA'; 
    'Vermont' = 'VT'; 'Washington' = 'WA'; 'Wisconsin' = 'WI'; 'West Virginia' =
    'WV'; 'Wyoming' = 'WY'; 'D.C.' = 'DC'")

# 1 = Legal in all or most cases (Pro-Choice opinion)
g2004$yes_of_all <- recode(g2004$Q32, "'Same-sex marriages' =  1; 
    'Civil unions but not same-sex marriages' = 0; 
    'Neither same-sex marriages nor civil unions' = 0; 'DK' = 0; else = 0")
g2004$yes_of_ans <- recode(g2004$Q32, "'Same-sex marriages ' = 1; 
    'Civil unions but not same-sex marriages' = 0; 
    'Neither same-sex marriages nor civil unions' = 0; 'DK' = 'NA'; else = 'NA'")

# Create index variables for individual-level model and poststratification
g2004$race_female <- (g2004$female * 3) + g2004$wbh
g2004$age_edu_cat <- 4 * (g2004$age - 1) + g2004$education
g2004$p_evang_full <- Statelevel$p.evang[g2004$state]
g2004$p_mormon_full <- Statelevel$p.mormon[g2004$state]
g2004$p_relig_full <- g2004$p_evang_full + g2004$p_mormon_full
g2004$p_kerry_full <- Statelevel$kerry.04[g2004$state]

# Fit individual-level model
# Poll parameter not estimated since model is estimating data from one poll
individual_model_gay04 <- glmer(formula = yes_of_all ~ (1|race_female) + 
    (1|age) + (1|education) + (1|age_edu_cat) + (1|state) + (1|region) + 
    p_relig_full + p_kerry_full, data = g2004, 
    family = binomial(link = "logit"))

# Model outputs probability that any adult will be pro-choice given the person's
# sex, race, age, education, and state
summary(individual_model_gay04)

# No respondents from Hawaii or Alaska
# Create vector of state random effects for Hawaii and Alaska (set equal to 0)
summary(g2004$STATE == "Hawaii")
summary(g2004$STATE == "Alaska")
state.ranefsgay04 <- array(NA, c(51,1))
dimnames(state.ranefsgay04) <- list(c(Statelevel$sstate), "effect")
for (i in Statelevel$sstate) {
    state.ranefsgay04[i,1] <- ranef(individual_model_gay04)$state[i,1]
}
state.ranefsgay04[,1][is.na(state.ranefsgay04[,1])] <- 0

# Compute weighted averages of pro-choice probability to estimate the proportion
# of pro-choice supporters in each state: for any specific cell j, specifying a 
# set of individual demographic and geographic values, the results of the 
# opinion model above allow us to make a prediction of pro-gay support,theta-j.
# 'cellpred' calculates weighted averages and then makes a prediction for each 
# demographic-state type.
cellpredgay04 <- invlogit(fixef(individual_model_gay04)["(Intercept)"]
     + ranef(individual_model_gay04)$race_female[census2000$crace_female,1]
     + ranef(individual_model_gay04)$age[census2000$cage.cat,1]
     + ranef(individual_model_gay04)$education[census2000$cedu.cat,1]
     + ranef(individual_model_gay04)$age_edu_cat[census2000$cage_edu_cat,1]
     + state.ranefsgay04[census2000$cstate,1]
     + ranef(individual_model_gay04)$region[census2000$cregion,1]
     + (fixef(individual_model_gay04)["p_relig_full"] * 
     census2000$cp_relig_full) + 
     fixef(individual_model_gay04)["p_kerry_full"]*census2000$cp_kerry_full)

# Weight the prediction by the frequency of each cell and calculate the percent 
# within each state (weighted average of responses)
cellpredweightedgay04 <- cellpredgay04 * census2000$cpercent.state 
statepredgay04 <- 100 * as.vector(tapply(cellpredweightedgay04, census2000$cstate, 
                                      sum))
statepredgay04
