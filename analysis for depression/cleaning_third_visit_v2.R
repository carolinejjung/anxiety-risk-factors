allclean <- read.csv(file.choose()) #original
colnames(allclean)
hist(allclean$depression_total_score)
hist(allclean$anxiety_perception)

thirdclean <- read.csv(file.choose()) #clean_third_visit.csv
colnames(thirdclean)
hist(thirdclean$anxiety_perception)

new <- thirdclean[,-c(1,2,5,44,45)]
#omit: part_id, clinical_visit, q_date, house_suitable (NA)

new$social_visits[4] <- mean(new$social_visits[-4]) #mean imputation for 999
new$raise_chair_time[15] <- mean(new$raise_chair_time[-15]) #mean imputation for 999

write.csv(new, "clean_third_visit_v2.csv", row.names=FALSE)


hist(allclean$anxiety_perception)
hist(allclean$anxiety_perception[allclean$clinical_visit==1])
hist(allclean$anxiety_perception[allclean$clinical_visit==2])
hist(allclean$anxiety_perception[allclean$clinical_visit==3])
