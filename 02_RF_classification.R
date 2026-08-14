# main
library(tidyverse)
library(tidytext)
library(stringi)

# calculations
library(doParallel)
library(randomForest)
library(caret)

#####################################
### random forest classification ###
#####################################

source("00_scripts.R")
### parallel setup

# How many cores to use in the cluster?
ncores <- 5
# Set up a cluster
my_cluster <- makeCluster(ncores)
# Register the cluster
registerDoParallel(my_cluster)

# list of packages, for foreach package
packages_used <- c("tidyverse", "doParallel", "tidytext", "randomForest", "stringi","caret")

##################
### classifier ###
##################


## each analysis has its own corpus (see data preparation in 01_main_analysis.R) and
## its own label handling: NA keeps metrical forms, "foot" collapses them in sample_range()
corp_dirs <- c(form="data/prepared", foot="data/prepared_foot")
label_t <- list(form=NA, foot="foot")

langs <- c("cs", "de","ru")
#langs <- langs[-1]
#cs,pos_syl,1000,2,1


n_lines = c(25,50,100, 200, 300, 500, 750, 1000)
ngram <- c(1,2)
features <- c("token", "pos", "syl", "pos_syl")


## analysis type
for(a_type in names(corp_dirs)) {

analysis_t <- label_t[[a_type]]
corp <- list.files(corp_dirs[[a_type]],full.names = T)
#  corp <- corp[-1]

## corpus
for(c in 1:length(corp)) {

  df <- read_tsv(corp[c]) %>%
    mutate(syl= str_remove_all(pos_syl, "[A-Z]"))

  foreach(i=1:100,
          .packages = packages_used) %dopar%  { 
            
            for(n in n_lines) {
              
              s <- sample_range(df,n_lines = n,n_samples_max = 10,label = analysis_t)
              
              for(f in features) {
                
                for(ng in ngram) {
                  
                  mff=NA
                  
                  if(f=="token") {
                    mff=100
                  }
                  
                  
                  if(f=="token" & ng==2 & n >= 50) {
                    mff=200
                  }
                  
                  
                  if(f=="pos_syl" & ng==2 & n >= 50) {
                    mff=200
                  }
                  
                  write_lines(paste0(langs[c], " ",
                                     f, " ",
                                     n, " ",
                                     ng, " ",
                                     a_type),file="log.txt",append = T)
                  
                  d <- vectorizer(s,mff = mff,ngram = ng,ftr = f,scale=T)
                  
                  ## RF
                  colnames(d) <- paste0("f_",colnames(d) %>% str_replace_all(" ", "_"))
                  rownames(d) <- rownames(d) %>% str_remove("_..?$")
                  
                  train <- d %>% as.data.frame() 
                  
                  train$meter1 <- as.factor(rownames(d))
                  
                  ## leave one out cross validation
                  #loo_res <- lapply(1:nrow(train), loo_cv, df=train) %>% unlist() %>% 
                   # factor(levels=levels(train$meter1))
                  rf_res <- rf_kfold(train,d,fold=0.2,n_folds = 25,iter = i)
                  acc <- rf_res[[1]] 
                  f1 <- rf_res[[2]]
                  #cm <- confusionMatrix(loo_res, train$meter1,mode = "prec_recall")
                  #acc <- cm$overall[1] # accuracy
                  #f1 <- mean(cm$byClass[,7]) %>% round(3) # macro F1
                  
                  # cm$table %>% write.table(file=paste0("results/conf_matrices/", 
                  #                                      langs[c], "_",
                  #                                      f, "_",
                  #                                      n, "_",
                  #                                      ng, "_",
                  #                                      analysis_t, "_",
                  #                                      i))
                
                  write_lines(file = paste0("results/res_",a_type,"_kfold.csv"), x=paste0(langs[c],",",f,",",n,",",ng, ",",acc,",",f1),append = T)
                  
                  
                }
              }
            }
          }
}
}

stopCluster(my_cluster)


######################
### session record ###
######################

write_session_info("02_RF_classification")


