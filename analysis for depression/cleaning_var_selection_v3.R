# STAT 228 Final Project
# Cleaning & variable selection v3
# 4/24/23

# read in original dataset
all_visits <- read.csv('Virtual Patient Models_Dataset.csv')

dim(all_visits)
names(all_visits)

table(all_visits$clinical_visit)
# 30 participants made 3 visits, 27 participants made all 4 visits

# ---------------------------------- #
# create binary indicator variable 
# 0 for depression scores < 5
# 1 otherwise

depressed_ind <- which(all_visits$depression_total_score >= 5)
depressed_ind 
new_col <- integer(dim(all_visits)[1])
new_col[depressed_ind] <- 1
new_col

all_visits_scored <- all_visits
all_visits_scored$depress_binary <- new_col
dim(all_visits_scored)
names(all_visits_scored)

# dataframe of patients with depression score >= 5
depressed <- all_visits_scored[all_visits_scored$depress_binary == 1,]
table(depressed$part_id)
# 7 participants had high depression score: 
# ID:              1088 1090 1100 1101 1104 2117 2584 
# num occurrences:    3    1    1    3    1    3    1 

# ---------------------------------- #
# choosing a single trial
all_visits$clinical_visit[all_visits$part_id == 2584 & all_visits_scored$depress_binary == 1] #input IDs
third_visit <- all_visits[all_visits$clinical_visit==3,]

# ---------------------------------- #
# assess missingness
missing <- is.na(third_visit)
missing_sum <- apply(missing, 2, sum)
names(all_visits_scored)[which(missing_sum > 0)]
# variables with missingness: 
# [1] "bmi_body_fat"   "lean_body_mass" "mna_total"      "stairs_number" 

# impute bmi_body_fat
third_visit$part_id[which(is.na(third_visit$bmi_body_fat))]
all_visits$bmi_body_fat[all_visits$part_id==2117] #missing all 4 trials
lm.bodyfat <- lm(bmi_body_fat~bmi_score, data=third_visit) #impute from regression
bodyfat2117 <- predict(lm.bodyfat, newdata=third_visit[third_visit$part_id==2117,])
third_visit$bmi_body_fat[third_visit$part_id==2117] <- bodyfat2117

bodyfat2584 <- all_visits$bmi_body_fat[all_visits$part_id==2584] #has one in 4th trial
third_visit$bmi_body_fat[third_visit$part_id==2584] <- bodyfat2584[4] #impute value for ID 2584

third_visit$bmi_body_fat[18] <- 21.8 #clean outlier

# impute lean_body_mass
third_visit$part_id[which(is.na(third_visit$lean_body_mass))]
third_visit$lean_body_mass[18] <- 78.7060 #clean outlier

lm.lean <- lm(lean_body_mass~bmi_score, data=third_visit) #impute from regression
lean2117 <- predict(lm.lean, newdata=third_visit[third_visit$part_id==2117,])
third_visit$lean_body_mass[third_visit$part_id==2117] <- lean2117

lean2584 <- all_visits$lean_body_mass[all_visits$part_id==2584] #has one in 4th trial
third_visit$lean_body_mass[third_visit$part_id==2584] <- lean2584[4] #impute value for ID 2584

# impute stairs_numbers
stairs <- all_visits$stairs_number[all_visits$clinical_visit==1]
third_visit$stairs_number <- stairs

#drop mna_total
mnaindex <- which(colnames(third_visit)=="mna_total")
third_visit2 <- third_visit[,-mnaindex]

# ---------------------------------- #
# export to csv
write.csv(third_visit2, "clean_third_visit.csv", row.names=FALSE)
