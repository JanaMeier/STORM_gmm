

############################################
###### Growth Mixture Modeling Script ######
############################################

# Script written by Jana Meier on 16/01/2023
# following this tutorial: https://psyarxiv.com/m58wx/ and this vignette: https://cecileproust-lima.github.io/lcmm/articles/latent_class_model_with_hlme.html 

## 0. SET UP -------------------------
rm(list=ls()) # clear workspace

# load required libraries
library(tidyverse)
library(here)
library(ggsignif)
library(janitor)
library(lcmm)
library(tictoc)


select <- dplyr::select

# LIR&JGU color sceme
colour3 = "#F7996E" #light salmon
colour2 = "#AFCA0B" #Celadon green
colour1 = "#7A7AE0" #Capri blue
colour4 = "#FFD966" #yellow
colour5 = "#B5594F" #dark red
colour6 <- "#DAA600" #dark yellow
colour7 <- "#6A7B07" #dark green

colours <- c(colour1, colour2, colour3, colour4)
colours_dark <- c(colour1, colour7, colour5, colour6)


## 1. LOAD DATA ---------------------------------------------------------------------

rating_data <- here("Data/rating_data_clean.txt") %>% 
  read_delim(delim = "\t") %>% 
  clean_names() %>% 
  filter(condition != "CSE_CC",
         id != "CSE_EC_04") %>%  # CSE_EC_04 was excluded but is still in the rating data set
  mutate(condition = factor(condition))


# 3. RUN LCMM MODELS ----------------------------------------------------------------

# create numeric subject variable (which lcmm demands)
subjects <- unique(rating_data$id)
i <- 1
for(subject in subjects){
  print(subject)
  rating_data$ID[rating_data$id == subject] <- i
  i <- i+1}

# set seed for random number generator so that results can be reproduced
set.seed(2002)

#tic()
# run models with 1-6 classes, each with 100 random starts, using the 1-class model to set initial start values:
# lcga1 <- hlme(control_rating ~ trial, subject = "ID", ng = 1, data = rating_data) # 1-class model
# lcga2 <- gridsearch(rep = 500, maxiter = 10, minit = lcga1, m = hlme(control_rating ~ trial, subject = "ID", ng = 2, data = rating_data, mixture = ~ trial))
# lcga3 <- gridsearch(rep = 500, maxiter = 10, minit = lcga1, m = hlme(control_rating ~ trial, subject = "ID", ng = 3, data = rating_data, mixture = ~ trial))
# lcga4 <- gridsearch(rep = 500, maxiter = 10, minit = lcga1, m = hlme(control_rating ~ trial, subject = "ID", ng = 4, data = rating_data, mixture = ~ trial))
# lcga5 <- gridsearch(rep = 500, maxiter = 10, minit = lcga1, m = hlme(control_rating ~ trial, subject = "ID", ng = 5, data = rating_data, mixture = ~ trial))
# lcga6 <- gridsearch(rep = 500, maxiter = 10, minit = lcga1, m = hlme(control_rating ~ trial, subject = "ID", ng = 6, data = rating_data, mixture = ~ trial))
# summary(lcga1)

# we fit multiple models with varying number of classes. The gridsearch()-function repeats the fitting process 100 times (should be larger, actually, to avoid finding a local instead of a global maximum) with random start values
# maxiter sets the maximum number of optimizations in each random start
# miniter sets the start values to be based on the 1-class model 

# make table with results for the models: 
#summarytable(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))

# We see that the BIC keeps decreasing, but it is unlikely that we need so many classes to adequately describe the data. 
# Also, from the 4-class model on there is a very small class (6.8%)
# So we might need another method to find the best fitting model. --> use a more flexible GMM.

# the main difference is that we add random intercepts per class by adding random = ~1 and nwg=T allows the variance of the random intercepts to vary across classes
# gmm1 <- hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 1, data = rating_data) # 1-class model
# gmm2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1, m = hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 2, data = rating_data, mixture = ~ trial, nwg = T))
# gmm3 <- gridsearch(rep = 100, maxiter = 10, minit = gmm1, m = hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 3, data = rating_data, mixture = ~ trial, nwg = T))
# gmm4 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1, m = hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 4, data = rating_data, mixture = ~ trial, nwg = T))
# gmm5 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1, m = hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 5, data = rating_data, mixture = ~ trial, nwg = T))
# gmm6 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1, m = hlme(control_rating ~ trial, subject = "ID", random = ~1, ng = 6, data = rating_data, mixture = ~ trial, nwg = T))

#summarytable(gmm1, gmm2, gmm3, gmm4, gmm5, gmm6, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))

# We try another model with random slopes:

# gmm1_2 <- hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 1, data = rating_data) # 1-class model
# gmm2_2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1_2, m = hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 2, data = rating_data, mixture = ~ trial, nwg = T))
# gmm3_2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1_2, m = hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 3, data = rating_data, mixture = ~ trial, nwg = T))
# gmm4_2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1_2, m = hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 4, data = rating_data, mixture = ~ trial, nwg = T))
# gmm5_2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1_2, m = hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 5, data = rating_data, mixture = ~ trial, nwg = T))
# gmm6_2 <- gridsearch(rep = 500, maxiter = 10, minit = gmm1_2, m = hlme(control_rating ~ trial, subject = "ID", random = ~1 + trial, ng = 6, data = rating_data, mixture = ~ trial, nwg = T))

#toc() # running the models with 500 random starts takes 3:45h, so better don't do this again
#summarytable(gmm1_2, gmm2_2, gmm3_2, gmm4_2, gmm5_2, gmm6_2, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))

# here, the model 3 class model has the lowest AIC and the 2 class model the lowest BIC but also the lowest entropy.
# But the 3-class model again has a very small class (6.8%) We keep finding this class in all models.

# Save models

# saveRDS(lcga1, file = here("Code/Growth_mixture_models/1_class_model_linear.rds"))
# saveRDS(lcga2, file = here("Code/Growth_mixture_models/2_class_model_linear.rds"))
# saveRDS(lcga3, file = here("Code/Growth_mixture_models/3_class_model_linear.rds"))
# saveRDS(lcga4, file = here("Code/Growth_mixture_models/4_class_model_linear.rds"))
# saveRDS(lcga5, file = here("Code/Growth_mixture_models/5_class_model_linear.rds"))
# saveRDS(lcga6, file = here("Code/Growth_mixture_models/6_class_model_linear.rds"))
# 
# saveRDS(gmm1, file = here("Code/Growth_mixture_models/1_class_model_intercepts_only.rds"))
# saveRDS(gmm2, file = here("Code/Growth_mixture_models/2_class_model_intercepts_only.rds"))
# saveRDS(gmm3, file = here("Code/Growth_mixture_models/3_class_model_intercepts_only.rds"))
# saveRDS(gmm4, file = here("Code/Growth_mixture_models/4_class_model_intercepts_only.rds"))
# saveRDS(gmm5, file = here("Code/Growth_mixture_models/5_class_model_intercepts_only.rds"))
# saveRDS(gmm6, file = here("Code/Growth_mixture_models/6_class_model_intercepts_only.rds"))
# 
# saveRDS(gmm1_2, file = here("Code/Growth_mixture_models/1_class_model_intercepts+slopes.rds"))
# saveRDS(gmm2_2, file = here("Code/Growth_mixture_models/2_class_model_intercepts+slopes.rds"))
# saveRDS(gmm3_2, file = here("Code/Growth_mixture_models/3_class_model_intercepts+slopes.rds"))
# saveRDS(gmm4_2, file = here("Code/Growth_mixture_models/4_class_model_intercepts+slopes.rds"))
# saveRDS(gmm5_2, file = here("Code/Growth_mixture_models/5_class_model_intercepts+slopes.rds"))
# saveRDS(gmm6_2, file = here("Code/Growth_mixture_models/6_class_model_intercepts+slopes.rds"))


# 4. LOAD MODELS  -----------------------------------------------------

lcga1 = readRDS(here("Code/GMM/Growth_mixture_models/1_class_model_linear.rds"))
lcga2 = readRDS(here("Code/GMM/Growth_mixture_models/2_class_model_linear.rds"))
lcga3 = readRDS(here("Code/GMM/Growth_mixture_models/3_class_model_linear.rds"))
lcga4 = readRDS(here("Code/GMM/Growth_mixture_models/4_class_model_linear.rds"))
lcga5 = readRDS(here("Code/GMM/Growth_mixture_models/5_class_model_linear.rds"))
lcga6 = readRDS(here("Code/GMM/Growth_mixture_models/6_class_model_linear.rds"))

gmm1 = readRDS(here("Code/GMM/Growth_mixture_models/1_class_model_intercepts_only.rds"))
gmm2 = readRDS(here("Code/GMM/Growth_mixture_models/2_class_model_intercepts_only.rds"))
gmm3 = readRDS(here("Code/GMM/Growth_mixture_models/3_class_model_intercepts_only.rds"))
gmm4 = readRDS(here("Code/GMM/Growth_mixture_models/4_class_model_intercepts_only.rds"))
gmm5 = readRDS(here("Code/GMM/Growth_mixture_models/5_class_model_intercepts_only.rds"))
gmm6 = readRDS(here("Code/GMM/Growth_mixture_models/6_class_model_intercepts_only.rds"))

gmm1_2 = readRDS(here("Code/GMM/Growth_mixture_models/1_class_model_intercepts+slopes.rds"))
gmm2_2 = readRDS(here("Code/GMM/Growth_mixture_models/2_class_model_intercepts+slopes.rds"))
gmm3_2 = readRDS(here("Code/GMM/Growth_mixture_models/3_class_model_intercepts+slopes.rds"))
gmm4_2 = readRDS(here("Code/GMM/Growth_mixture_models/4_class_model_intercepts+slopes.rds"))
gmm5_2 = readRDS(here("Code/GMM/Growth_mixture_models/5_class_model_intercepts+slopes.rds"))
gmm6_2 = readRDS(here("Code/GMM/Growth_mixture_models/6_class_model_intercepts+slopes.rds"))


# 5. EVALUATE THE MODELS:  -----------------------------------------

## 5.1 Overview ---------------------------

# first look at a unitary growth curve model (model with no random effects and only one class)
summary(lcga1)
estimates(lcga1)

# now look at all the different models:
summarytable(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))
summarytable(gmm1, gmm2, gmm3, gmm4, gmm5, gmm6, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))
summarytable(gmm1_2, gmm2_2, gmm3_2, gmm4_2, gmm5_2, gmm6_2, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))

summaryplot(lcga1, lcga2, lcga3, lcga4, lcga5, lcga6)
summaryplot(gmm1, gmm2, gmm3, gmm4, gmm5, gmm6)
summaryplot(gmm1_2, gmm2_2, gmm3_2, gmm4_2, gmm5_2, gmm6_2)


# 5.2 LCGA with no random effects, 4 classes ----------------------------------------------
# Lowest BIC, entropy 0.9, but one class has only 6.9%

summary(lcga4) # model summary
pred0 <- predictY(lcga4, rating_data, var.time = "trial", draws = T) # compute predictions
plot(pred0, ylab="control_rating",  main="Predicted trajectories for perceived control ", shades = T) # plot predictions
plot(lcga4, cex.main=0.8) # plot residuals

# 5.3 GMM with random intercepts, 3 classes ---------------------------------------------
# lowest BIC of random intercept models, bad entropy (0.71), all classes have reasonable sizes

summary(gmm3) # model summary 
#plot(gmm3, which = "postprob") # distribution of posterior classification
pred0 <- predictY(gmm3, rating_data, var.time = "trial", draws = T)
predictions <- cbind(pred0$times, pred0$pred) %>% pivot_longer(cols = Ypred_class1:upper.Ypred_class3, names_to = c(".value", "class"), names_pattern = "(.*)_(.*)")
predictions %>% ggplot(aes(x=trial, y=Ypred, colour=class, fill=class, shape=class, group = class, ymin=lower.Ypred, ymax=upper.Ypred)) +
  geom_ribbon(alpha=0.2) +
  geom_line() +
  geom_point(size=3) +
  theme_bw() + scale_colour_manual(values=class_colours) +scale_fill_manual(values=class_colours) +
  labs(title="Estimated mean trajectories of final solution", y="Predicted control rating", x="Trial")

ggsave("Estimated_trajectories.png", path=here("Output"), width=5, height=4, dpi=340)

plot(gmm3, cex.main=0.8) # plot residuals

# 5.5 GMM with random intercepts and slopes, 2 classes ----------------------------------------
# lowest BIC of random slopes models, ok entropy (0.76), all classes have reasonable sizes

summary(gmm2_2) # model summary
pred0 <- predictY(gmm2_2, rating_data, var.time = "trial", draws = T) # compute predictions
plot(pred0, ylab="control_rating",  main="Predicted trajectories for perceived control ", shades = T) # plot predictions

# 5.6 GMM with random intercepts and slopes model, 3 classes ---------------------------------
# lowest AIC of random slopes models, ok entropy (0.85), one class with only 7%

summary(gmm3_2)
pred0 <- predictY(gmm3_2, rating_data, var.time = "trial", draws = T)
plot(pred0, ylab="control_rating",  main="Predicted trajectories for perceived control ", shades = T)

## 6. OUTPUT SELECTED MODELS ---------------------------------------------------

# 6.1 Get posterior classification ------------------------------------
postprob(lcga4) 
postprob(gmm3)
postprob(gmm2_2)
postprob(gmm3_2)

# 6.2 Get class IDs and probabilities and write csv --------------------------

id_4l <- lcga4$pprob
id_3i <- gmm3$pprob
id_2s <- gmm2_2$pprob
id_3s <- gmm3_2$pprob

ids <- rating_data %>% select(id, ID) %>% unique() # create a dataframe with the real study IDs and the numeric model IDs for matching

classes <- id_4l %>% full_join(id_3i, "ID") %>%  full_join(id_2s, "ID") %>% full_join(id_3s, "ID") # join the class probabilities for different models
# correct the variable names
colnames(classes) <- c("ID", "class_4l", "prob1_4l", "prob2_4l", "prob3_4l", "prob4_4l", "class_3i", "prob1_3i", "prob2_3i", "prob3_3i", "class_2s", "prob1_2s", "prob2_2s", "class_3s", "prob1_3s", "prob2_3s", "prob3_3s")
classes <- ids %>% left_join(classes) %>% select(!ID) # add real IDs to class-probabilities again

#write.csv(classes, here("Data/latent_class_data.csv"), row.names = F) # save the data frame with the class probabilities























