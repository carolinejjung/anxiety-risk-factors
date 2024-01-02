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
# use data from the third visit only

first_visit <- all_visits[all_visits$clinical_visit==1,]
second_visit <- all_visits[all_visits$clinical_visit==2,]
third_visit <- all_visits[all_visits$clinical_visit==3,]

# distribution from second visit most closely matches overall
par(mfrow=c(2,2))
hist(all_visits$anxiety_perception)
hist(first_visit$anxiety_perception, breaks=10)
hist(second_visit$anxiety_perception, breaks=10)
hist(third_visit$anxiety_perception, breaks=10)

# BUT third trial has least missingness
missing <- (is.na(first_visit) | first_visit == 999)
missing_sum <- apply(missing, 2, sum)
missing_sum[which(missing_sum > 0)]

missing <- (is.na(second_visit) | second_visit == 999)
missing_sum <- apply(missing, 2, sum)
missing_sum[which(missing_sum > 0)]

missing <- (is.na(third_visit) | third_visit == 999)
missing_sum <- apply(missing, 2, sum)
missing_sum[which(missing_sum > 0)]
# variables with missingness: 
# raise_chair_time     bmi_body_fat   lean_body_mass        mna_total 
#                1                2                2               30 
#    social_visits    stairs_number 
#                1               30 


third_visit <- all_visits[all_visits$clinical_visit==3,]

# ---------------------------------- #
# impute bmi_body_fat
third_visit$part_id[which(is.na(third_visit$bmi_body_fat))]

# impute from regression
all_visits$bmi_body_fat[all_visits$part_id==2117] # missing all 4 trials

plot(third_visit$bmi_body_fat, third_visit$bmi_score)
third_visit$bmi_body_fat[18] <- 21.8 #clean outlier

lm.bodyfat <- lm(bmi_body_fat~bmi_score, data=third_visit) 
bodyfat2117 <- predict(lm.bodyfat, newdata=third_visit[third_visit$part_id==2117,])
third_visit$bmi_body_fat[third_visit$part_id==2117] <- bodyfat2117

# impute from fourth visit
bodyfat2584 <- all_visits$bmi_body_fat[all_visits$part_id==2584] 
third_visit$bmi_body_fat[third_visit$part_id==2584] <- bodyfat2584[4] 

# ---------------------------------- #
# impute lean_body_mass
third_visit$part_id[which(is.na(third_visit$lean_body_mass))]

# impute from regression
plot(third_visit$lean_body_mass, third_visit$bmi_score)
third_visit$lean_body_mass[18] <- 78.7060 #clean outlier

lm.lean <- lm(lean_body_mass~bmi_score, data=third_visit)
lean2117 <- predict(lm.lean, newdata=third_visit[third_visit$part_id==2117,])
third_visit$lean_body_mass[third_visit$part_id==2117] <- lean2117

# impute from fourth visit
lean2584 <- all_visits$lean_body_mass[all_visits$part_id==2584] 
third_visit$lean_body_mass[third_visit$part_id==2584] <- lean2584[4] 

# ---------------------------------- #
# impute stairs_numbers
stairs <- all_visits$stairs_number[all_visits$clinical_visit==1]
third_visit$stairs_number <- stairs

#drop mna_total
mnaindex <- which(colnames(third_visit)=="mna_total")
third_visit <- third_visit[,-mnaindex]

# ---------------------------------- #
# deal with 999 values in raise_chair_time and social_visits 
out_ind <- which(third_visit$raise_chair_time == 999)
hist(third_visit$raise_chair_time[-out_ind])
third_visit$raise_chair_time[out_ind] <- mean(third_visit$raise_chair_time[-out_ind])

out_ind <- which(third_visit$social_visits == 999)
hist(third_visit$social_visits[-out_ind])
third_visit$social_visits[out_ind] <- mean(third_visit$social_visits[-out_ind])

# remove house_suitable_professional and house_suitable_professional
names(third_visit)
third_visit <- third_visit[,-c(44, 45)]

# ---------------------------------- #
# double check missingness
missing <- (is.na(third_visit) | third_visit == 999)
missing_sum <- apply(missing, 2, sum)
missing_sum[which(missing_sum > 0)]

# export to csv
write.csv(third_visit, "clean_v4.csv", row.names=FALSE)
