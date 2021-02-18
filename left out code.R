
# Another method (ratio 70:30)
set.seed(400) # To always run same answer; or just put seed
train <- sample(nrow(data_uganda), 0.7*nrow(data_uganda), replace = FALSE)
data_uganda_train_v2 <- data_uganda[train,] #  Train is like the data we would collect in a randomized experiment 
data_uganda_valid_v2 <- data_uganda[-train,] # Test would be the future cases which we are trying to predict
summary(data_uganda_train_v2)
nrow(data_uganda_train_v2)
summary(data_uganda_valid_v2)
nrow(data_uganda_valid_v2)

#most baseline variable that exist in data
X_baseline <- model.matrix(lm(Y_outcome ~ children + children_na +
                                branchno + branchno_na +
                                age + age_na +
                                rural + rural_na +
                                income + income_na + 
                                enroll_school + enroll_school_na +
                                study_hours+ study_hours_na + 
                                index_empowerment + index_empowerment_na +
                                att_earn_moneyfam + att_earn_moneyfam_na +
                                z_Entrep_total + z_Entrep_total_na +
                                z_Expenditure_totDF + z_Expenditure_totDF_na +
                                value_assets + value_assets_na +
                                loan_brac + loan_brac_na +
                                ablework_married + ablework_married_na +
                                M_marrywhen + M_marrywhen_na +
                                who_decidehusband + who_decidehusband_na +
                                work_married + work_married_na +
                                want_respect + want_respect_na +
                                worry_job + worry_job_na +
                                satisf_income + satisf_income_na +
                                selfempl + selfempl_na +
                                hsworked_year_empl + hsworked_year_empl_na +
                                hsworked_year_self + hsworked_year_self_na +
                                life_skills + life_skills_na +
                                livelihood + livelihood_na +
                                enroll_school*study_hours + 
                                enroll_school_na*study_hours_na +
                                children*age +
                                children_na*age_na,
                              data = data_uganda_train))


Causal forest- based on the paper
```{r}
Y.forest.basic = regression_forest(X_vars, Y_outcome, clusters = C_vars)
Y.hat.basic = predict(Y.forest.basic)$predictions

W.forest.basic = regression_forest(X_vars, 
                                   W_treatment, 
                                   clusters = C_vars)
W.hat.basic = predict(W.forest.basic)$predictions
#train causal forest
cf.basic <- causal_forest(X = X_vars,
                          Y = Y_outcome,
                          W = W_treatment, 
                          Y.hat = Y.hat.basic, 
                          W.hat = W.hat.basic, 
                          num.trees = 60000,
                          clusters = C_vars,
                          orthog.boosting = TRUE,
                          tune.parameters = "all")# paper clusters

# but see again!!! Athey and Wager 2019; orthogonalization
##check for variable importance
varimp.basic = variable_importance(cf.basic)
selected.idx.basic = which(varimp.basic > mean(varimp.basic)) #select only important variables and train another causal forest using only important variables
selected.idx.basic

#train basic model with selected imoprtant variables
cf.basic.imp <- causal_forest(X = X_vars[,selected.idx.basic],
                              Y = Y_outcome,
                              W = W_treatment, 
                              Y.hat = Y.hat.basic, 
                              W.hat = W.hat.basic, 
                              num.trees = 60000,
                              clusters = C_vars,
                              orthog.boosting = TRUE,
                              tune.parameters = "all")

X.test.basic <- model.matrix(~ children + children_na +
                               branchno + branchno_na +
                               age + age_na +
                               rural + rural_na +
                               income + income_na + 
                               enroll_school + enroll_school_na +
                               study_hours+ study_hours_na + 
                               index_empowerment + index_empowerment_na +
                               enroll_school*study_hours + 
                               enroll_school_na*study_hours_na +
                               children*age +
                               children_na*age_na,
                             data= data_uganda_test)


## Predict the test data
# Tell grf to include variance estimates, and then I assign the predictions (the estimated treatment effects) to the test data frame so that we can use these in subsequent analyses
pred.cf.basic.imp <- predict(object = cf.basic.imp, 
                             newdata = X.test.basic[,selected.idx.basic],
                             data= data_uganda_test, 
                             estimate.variance = TRUE)
pred.cf.basic.imp
```

#Finds the optimal ridge penalty for local linear causal prediction.
#A list of lambdas tried, corresponding errors, and optimal ridge penalty lambda.
tune_ll_causal_forest(cf,
                      linear.correction.variables = NULL,
                      ll.weight.penalty = FALSE,
                      num.threads = NULL,
                      lambda.path = NULL
)



#absolute error rate = 9% of the predictions are completely wrong at the moment
ae <- ae(data_uganda_test$foll_index_income_gen_act,
         pred.cf.basic.imp$predictions)
ae
