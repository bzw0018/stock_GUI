rm(list = ls()) # clear environment
cat("\014") # clear console
#_________________________________operational functions definition__________________________________
row_missing_function <- function(input_data){
  na_count_row <- apply(input_data, 1, function(y) length(which(is.na(y)))/ncol(input_data)) # counting nas in each row
  na_count_row <- data.frame(na_count_row) #transforming this into a data_frame
  return(na_count_row)
}

col_missing_function <- function(input_data){
  na_count_col <- apply(input_data, 2, function(y) length(which(is.na(y)))/nrow(input_data)) # counting nas in each column
  na_count_col <- data.frame(na_count_col) #transforming this into a data_frame
  return(na_count_col)
}

# The the invariant function

invariant_checker <- function(vector){
  vector_sort = sort(vector)
  if(head(vector_sort,1) == tail(vector_sort,1)){
    return("Two")
  }else{
    return("No")
  }
}

#_____________
# Three group function
class_generator_multi <- function(gstatus,gtime,g1,g2,g3,g4){
  g1<-as.numeric(g1)*365
  g2<-as.numeric(g2)*365
  g3<-as.numeric(g3)*365
  g4<-as.numeric(g4)*365
  
  gmax<-max(g1,g2,g3,g4)
  g_cat_max<-gmax/365
  if(gstatus ==1){
    if(gtime <= g1){return(1)}
    if(gtime > g1 & gtime <= g2){return(2)}
    if(gtime > g2 & gtime <= g3){return(3)}
    if(gtime > g3 & gtime <= g4){return(4)}
    if(gtime > gmax){return(g_cat_max)}
    
  }else{
    if(gtime>gmax){
      return(5)
    }else{
      return(NA)
    }
  }
}
#_________________________
# TARGET generator for Bionomial
target_generator <- function(gstatus, gtime, year){
  if(gtime < year*365){
    if(gstatus == 0){
      return(NA)
    }else {
      return(1)
    }
  }else{
    return(0)
  }
}
#_________________________
#finding appreance of one dataset's rows in another dataset, 
#The variables column of each dataset should be the interesting elements

var_finder <- function(input1,input2){
  
  if(is.null(input1$variables)){input1$variables<-input1[1]}
  seeker<-as.data.frame(input1$variables)
  names(seeker)<-"variables"
  seeker$results<-NA
  
  if(is.null(input2$variables)){input2$variables<-input2[1]}  
  seeked<-as.data.frame(input2$variables)
  names(seeked)<-"variables"
  
  for (i in 1:nrow(seeker)){
    for (j in 1:nrow(seeked)) {
      if(grepl(seeker$variables[i],seeked$variables[j])){
        if(str_count(seeker$variables[i],"_")==str_count(seeked$variables[j],"_")){
          seeker$results[i]<-"X"}
      }
    }
  }    
  seeker<-seeker[complete.cases(seeker),]
  seeker$results<-NULL
  
  return(seeker)
  
}

#finding importance of variables from a model

model_imp_exc <- function(input_object,vars_org){
  imp_model<-varImp(input_object)
  imp_model<-as.data.frame(imp_model$importance)
  imp_model$variables<-rownames(imp_model)
  imp_model$importance<-as.numeric(imp_model[,1])
  
  model_imp<-matrix(0, ncol = 1, nrow = nrow(imp_model))
  model_imp<-as.data.frame(model_imp)
  
  
  model_imp$variables<-imp_model$variables
  model_imp$importance<-imp_model$importance
  model_imp$V1<-NULL 
  
  seeker<-model_imp
  seeker$real_name<-NA
  
  seeked<-as.data.frame(vars_org[,1])
  names(seeked)<-"variables"
  
  for (i in 1:nrow(seeker)){
    for (j in 1:nrow(seeked)) {
      if(grepl(seeked$variables[j],seeker$variables[i])){
        if(str_count(seeker$variables[i],"_")==str_count(seeked$variables[j],"_")){
          seeker$real_name[i]<-as.character(seeked$variables[j])}
      }
    }
  }
  # It's for the variables that created by us and are not in the original dataset
  for (i in 1:nrow(seeker)){
    if(is.na(seeker$real_name[i])){seeker$real_name[i]<-seeker$variables[i]}
  }
  str(seeker$importance)
  res.by <- by(seeker$importance, seeker$real_name, mean)
  res.by
  
  seeker<-as.data.frame(res.by)
  names(seeker)<-c("variables","importance")
  
  return(seeker) 
}

###################filling a matrix from another matrix based on ellements of the common matrix
# col_no is index of the column that we want to get filled
# filling_matrix is the matrix that has empty columns
# filler_matrix is a n*2 matrix wich has an index column and values in the second column
# common_col is the common column that has indeces for matching the rows
matrix_filler<-function(col_no,filling_matrix,filler_matrix,common_col){
  common_col<-as.character(common_col)
  
  filler_col<-match(common_col,names(filler_matrix))
  filling_col<-match(common_col,names(filling_matrix))
  
  if(filler_col-2==0){
    values<-1
  }else{values<-2}
  
  for (k in 1:nrow(filling_matrix)){
    for (j in 1:nrow(filler_matrix)) {
      if(grepl(filler_matrix[j,filler_col],filling_matrix[k,filling_col])){
        if(str_count(filling_matrix[k,filling_col],"_")==str_count(filler_matrix[j,filler_col],"_")){
          filling_matrix[k,col_no]<-filler_matrix[j,values]}
      }
    }
  }        
  return(filling_matrix) 
}

#__________________________________converting categorical variables to dummy variables_____________________
dummy_maker<-function(data,begin,end){
  for(i in begin:end){
    if(class(data[,i])!="numeric"){data[,i]<-as.factor(data[,i])}
  }
  new_cols<-matrix(0, ncol = 1, nrow = nrow(data))
  new_cols<-as.data.frame(new_cols)
  new_cols[,1]<-NULL
  T<-NULL
  
  for(i in begin:end){
    if(nlevels(data[,i])>1){
      formula<-as.formula(paste("~",colnames(data)[i],"-1",collapse = " "))
      dummy_matrix<-as.data.frame(model.matrix(formula, data=data ))
      dummy_matrix[,1]<-NULL
      new_cols<-cbind(new_cols,dummy_matrix)
      T<-c(T,i)
    }
  }
  if(!is.null(T)){
    data<-data[ ,-T]
    data<-cbind(new_cols,data)}
  return(data)
}
pre_process<-function(x){
  # removing near zero columns
  nzv<-nearZeroVar(x)
  x<- x[,-nzv]
  #removing highly correlated columns
  descrCor <- cor(x)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
  x <- x[, -highlyCorDescr]
  #removing columns that have linear relationship
  comboInfo <- findLinearCombos(x)
  if(!is.null(comboInfo$remove)){x<-x[, -comboInfo$remove]}
  return(x)}
#=========================================================================================================
#     Reading Module
#=========================================================================================================
reading <- function(inputss) {
  
  NA_cleaning<-"kk"
  RF_Method<-"kk"
  Lasso_Method<-"kk"
  itself_Method<-"kk"
  lit_Method<-"kk"
  tuning<-"kk"
  conso_feat<-"kk"
  lit_vars<-"kk"
  n_folds<-"kk"
 
 

  platform<-inputss$platform
  Multi_Bino<-inputss$Multi_Bino
  if(Multi_Bino=="Multinomial"){is_multi<-"Mult"}else{is_multi<-""}

  method_fs<-inputss$method_fs
  
  FS_Method_RF=regexpr(pattern ='Random Forrest',method_fs)[1]
  FS_Method_Lasso=regexpr(pattern ='Lasso',method_fs)[1]
  FS_Method_FCBF=regexpr(pattern ='Fast Selection',method_fs)[1]
  FS_Method_itself=regexpr(pattern ='itself',method_fs)[1]

  
  Lasso_run<-inputss$Lasso_run
  FCBF_run<-inputss$FCBF_run
  itself_run<-inputss$itself_run
  RF_run<-inputss$RF_run

  lit_vars<-inputss$lit_vars
  n_folds<-inputss$n_folds
  n_folds<-as.numeric(n_folds)

  balanc_proc<-inputss$balanc_proc
  B_alg<-inputss$B_alg
  
  H_por_low<-inputss$H_por_low
  H_por_low<-as.numeric(H_por_low)
  
  H_por_high<-inputss$H_por_high
  H_por_high<-as.numeric(H_por_high)
  
  file_name<-inputss$file_name
  conso_feat<-inputss$conso_feat
  root_folder_input<-inputss$root_folder_input
  if(is.null(root_folder_input)){root_folder<-getwd()}else{root_folder<-root_folder_input}
  analysis_folder<-paste(root_folder,"/analysis/",sep="")
  email<-inputss$email

  target<-inputss$target
  uservars<-inputss$uservars
  tuning_input<-inputss$tuning_input
  if(tuning_input=="Yes"){tuning<-"tuned"}

  if(conso_feat=="Yes"){consolidate<-"consolidated"}else{consolidate<-""}
  is_bal<-"No-balancing"
  if(balanc_proc=="Yes"){
    if(B_alg=="Smote"){is_bal<-"Smote"}
    if(B_alg=="RUS"){is_bal<-"RUS"}
    if(B_alg=="Hybrid"){is_bal<-"Hybrid"}
  }else{is_bal<-"No-balancing"}
  input_object<-list()
  input_object$data_base<-inputss$data_base
  input_object$dataset<-inputss$dataset
  input_object$vars_independant<-uservars
  input_object$lit_vars<-lit_vars

  params<-c(platform,NA_cleaning,Multi_Bino,is_multi,method_fs,FS_Method_RF,FS_Method_Lasso,
            FS_Method_FCBF,FS_Method_itself, Lasso_run,FCBF_run,itself_run,RF_run,
                n_folds,balanc_proc,B_alg,H_por_low,H_por_high,file_name,conso_feat,root_folder,
            analysis_folder,email,target,tuning_input,tuning,consolidate,is_bal)

  params<-as.data.frame(params)
  params[]<- lapply(params, as.character)
  row.names(params)<-c("platform","NA_cleaning","Multi_Bino","is_multi","method_fs","FS_Method_RF","FS_Method_Lasso",
                       "FS_Method_FCBF","FS_Method_itself","Lasso_run","FCBF_run","itself_run","RF_run",
                           "n_folds","balanc_proc","B_alg","H_por_low","H_por_high","file_name","conso_feat","root_folder",
                       "analysis_folder","email","target","tuning_input","tuning","consolidate","is_bal")

  names(params)<-c("content")

  input_object$parameters<-params
  
  
  return(input_object)
}

#==========================================================================================================
#      The Data prepararion function is defined here
#==========================================================================================================
initialization<-function(input_object){

  #__________________________________Path Initialization_____________________________
  
  root_folder<-input_object$parameters["root_folder","content"] 
  analysis_folder<-input_object$parameters["analysis_folder","content"]
  is_multi<-input_object$parameters["is_multi","content"]
  is_bal<-input_object$parameters["is_bal","content"]
  file_name<-input_object$parameters["file_name","content"]
  trg<-input_object$parameters["target","content"]
  method_fs<-input_object$parameters["method_fs","content"]
  consolidate<-input_object$parameters["consolidate","content"]
  tuning<-input_object$parameters["tuning","content"]
  Multi_Bino<-input_object$parameters["Multi_Bino","content"]
  RF_run<-input_object$parameters["RF_run","content"]
  Lasso_run<-input_object$parameters["Lasso_run","content"]
  FCBF_run<-input_object$parameters["FCBF_run","content"]
  itself_run<-input_object$parameters["itself_run","content"]
  lit_vars<-input_object$parameters["lit_vars","content"]

  Random_Forrest_Location<-paste(c(root_folder,"/","Random_Forrest_",is_multi,trg,"_variables_",file_name),collapse = "")
  Lasso_location<-paste(c(root_folder,"/","Lasso_",is_multi,trg,"_variables_",file_name),collapse = "")
  Fast_Feature_Location<-paste(c(root_folder,"/","Fast_Feature_",is_multi,trg,"_variables_",file_name),collapse = "")
  Literature_file<-paste(c(root_folder,"/","Literature_",trg,"_Litrature_Review_",file_name),collapse = "")
  consolidate_Location<-paste(c(root_folder,"/","consolidate_",is_multi,trg,"_variables_",file_name),collapse = "")

  base_file<-paste(c(root_folder,"/",file_name),collapse = "")
  Out_put_file_name<-paste(c(file_name,"_",trg,"_",method_fs,"_",consolidate,"_",is_bal,"_",is_multi,"_",tuning,".txt"),collapse = "")
  Out_put_file<-paste(c(analysis_folder,file_name,"_",trg,"_",method_fs,"_",consolidate,"_",is_bal,"_",is_multi,"_",tuning,".txt"),collapse = "")

  prediction_data<-paste(c(root_folder,"/","prediction_data","_",method_fs,"_",consolidate,"_","_",is_multi,"_",is_bal,"_",file_name,"_",trg,"_",tuning,".csv"),collapse = "")

  log_imp_folds_file<-paste(c(analysis_folder,"importance/","log_imp_vars_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  svm_imp_folds_file<-paste(c(analysis_folder,"importance/","svm_imp_vars_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  CRT_imp_folds_file<-paste(c(analysis_folder,"importance/","CRT_imp_vars_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  NNET_imp_folds_file<-paste(c(analysis_folder,"importance/","NNET_imp_vars_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  tan_imp_folds_file<-paste(c(analysis_folder,"importance/","tan_imp_vars_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")

  log_perf_folds_file<-paste(c(analysis_folder,"performance/","log_perf_folds_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  svm_perf_folds_file<-paste(c(analysis_folder,"performance/","svm_perf_folds_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  CRT_perf_folds_file<-paste(c(analysis_folder,"performance/","CRT_perf_folds_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  NNET_perf_folds_file<-paste(c(analysis_folder,"performance/","NNET_perf_folds_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  tan_perf_folds_file<-paste(c(analysis_folder,"performance/","tan_perf_folds_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")

  Avg_log_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_log_perf_folds_","_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  Avg_svm_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_svm_perf_folds_","_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  Avg_CRT_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_CRT_perf_folds_","_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  Avg_NNET_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_NNET_perf_folds_","_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")
  Avg_tan_perf_folds_file<-paste(c(analysis_folder,"performance/","Avg_tan_perf_folds_","_",is_multi,"_",method_fs,"_",consolidate,"_",is_bal,"_",trg,"_",tuning,file_name),collapse = "")

  itself_svm_Location<-paste(c(root_folder,"/","itself_svm_variables_",is_bal,"_",is_multi,"_",trg,"_",tuning,"_",file_name),collapse = "")
  itself_log_Location<-paste(c(root_folder,"/","itself_log_variables_",is_bal,"_",is_multi,"_",trg,"_",tuning,"_",file_name),collapse = "")
  itself_CRT_Location<-paste(c(root_folder,"/","itself_CRT_variables_",is_bal,"_",is_multi,"_",trg,"_",tuning,"_",file_name),collapse = "")
  itself_NNET_Location<-paste(c(root_folder,"/","itself_NNET_variables_",is_bal,"_",is_multi,"_",trg,"_",tuning,"_",file_name),collapse = "")
  itself_tan_Location<-paste(c(root_folder,"/","itself_tan_variables_",is_bal,"_",is_multi,"_",trg,"_",tuning,"_",file_name),collapse = "")

  # _______________________________Output setup________________________________________
  
  if(Multi_Bino=="Multinomial"){Method_output<-"Multinomial"}else{Method_output<-"Binomial"}
  FS_Method_output<-method_fs

  settings<-c(Random_Forrest_Location,Lasso_location,consolidate_Location,
              Fast_Feature_Location,base_file,Out_put_file,
              Literature_file,prediction_data,consolidate,
              log_imp_folds_file,svm_imp_folds_file,CRT_imp_folds_file,NNET_imp_folds_file,tan_imp_folds_file,
              log_perf_folds_file,svm_perf_folds_file,CRT_perf_folds_file,NNET_perf_folds_file,tan_perf_folds_file,
              Avg_log_perf_folds_file,Avg_svm_perf_folds_file,Avg_CRT_perf_folds_file,
              Avg_NNET_perf_folds_file,Avg_tan_perf_folds_file,Method_output,FS_Method_output,
              itself_svm_Location,itself_log_Location,itself_CRT_Location,itself_tan_Location,
              itself_NNET_Location,Out_put_file_name)
  
  settings<-as.data.frame(settings)
  settings[] <- lapply(settings, as.character)
  row.names(settings)<-c("Random_Forrest_Location","Lasso_location","consolidate_Location",
                         "Fast_Feature_Location","base_file","Out_put_file",
                         "Literature_file","prediction_data","consolidate",
                         "log_imp_folds_file","svm_imp_folds_file","CRT_imp_folds_file","NNET_imp_folds_file","tan_imp_folds_file",
                         "log_perf_folds_file","svm_perf_folds_file","CRT_perf_folds_file","NNET_perf_folds_file","tan_perf_folds_file",
                         "Avg_log_perf_folds_file","Avg_svm_perf_folds_file","Avg_CRT_perf_folds_file",
                         "Avg_NNET_perf_folds_file", "Avg_tan_perf_folds_file","Method_output","FS_Method_output",
                         "itself_svm_Location","itself_log_Location","itself_CRT_Location","itself_tan_Location",
                         "itself_NNET_Location","Out_put_file_name")
  names(settings)<-"content"

  input_object$settings<-settings
  
  return(input_object)
}

#==========================================================================================================
#      The Data Cleaning
#==========================================================================================================

data_cleaning<-function(input_object){
  
  #_______________________________Required Libraries_______________________________________________________________________________________
  # Install any needed package with the following command: install.packages("Name", dependencies = c("Depends", "Suggests"))
  
  library(haven); #install.packages("haven") to be able to read the STATA file .DTA
  library(ggplot2); library(caret); library(mlbench); library(gmodels); library(doParallel)
  registerDoParallel(); getDoParWorkers(); library(bnclassify); library(devtools); library(mailR)
  library(party); library(grid); library(mvtnorm); library(modeltools); library(stats4); library(strucchange)
  library(zoo); library(sandwich); library(kernlab); library(nnet); library(randomForest); library(caret); library(pROC); library(AUC)
  library(gdata); library(data.table); library(plyr); library(xlsx); library(stringr); library(WriteXLS); library(e1071); library(xlsx)
  library(arules); library(DMwR); library(ROSE);library(taRifx);
  if(input_object$parameters["platform","content"]=="MAC OS"){library(doMC)}

  set.seed(110)
  #________________________________Initilization___________________________________________________________________________________________

  Try_data_base<-input_object$data_base
  Try_data<-input_object$dataset
  TARGET<-input_object$parameters["target","content"]
  Try_data$TARGET<-Try_data_base[,TARGET]
  Try_data$TARGET<-as.factor(Try_data$TARGET)

  levels(Try_data$TARGET)[1]<-"down"
  levels(Try_data$TARGET)[2]<-"up"

  invariant_result <- list()

  for(i in 1:ncol(Try_data)){
    invariant_temp = Try_data[,i]
    invariant_result_temp = invariant_checker(invariant_temp)
    invariant_result = c(invariant_result, invariant_result_temp)
  }
  
  invariant_index <- which(invariant_result == "Two")

  if(length(invariant_index)>0){
    Try_data <- Try_data[, -invariant_index]}

  input_object$dataset<-Try_data
 # converting variables and column names to uppercase
  VarsData <- as.data.frame(input_object$vars_independant)
  VarsData<-as.data.frame(VarsData)
  names(VarsData)<-"variables"
  VarsData_Phase <- VarsData
  VarsData_Phase$selected<-NA

  input_object$VarsData_Phase<-VarsData_Phase
  
  return(input_object)
  
}#End of the Data prepararion function

#==========================================================================================================
#      The Data cleaning function ends here
#      Feature Selection Part Starts Here
#==========================================================================================================

#=============================================
#==============Random Forest Feature Selection
#=============================================

Random_Forrest<-function(input_object){
  if(as.numeric(input_object$parameters["FS_Method_RF","content"]>0)){
    if(input_object$parameters["RF_run","content"]=="No"){
      library(Boruta)
      
      # formula<-paste(input_object$parameters["target","content"],"~.",sep="")
      # formula<-as.formula(formula)
      #cat("The Random Forest Feature Selection is started here \n")
      # d_base<-as.data.frame(parameters$data_base)
      # d_data<-as.data.frame(parameters$data)
      write.csv(input_object$data_base,"/Users/hamid/Dropbox/My R codes/Bin-Hamid/Stock_Prob/aaa_data_base.csv")
      write.csv(input_object$dataset,"/Users/hamid/Dropbox/My R codes/Bin-Hamid/Stock_Prob/aaa_data.csv")
      write.csv(input_object$vars_independant,"/Users/hamid/Dropbox/My R codes/Bin-Hamid/Stock_Prob/aaa_data.csv")
      Random_Forrest.train <- Boruta(TARGET~., data = input_object$dataset, doTrace = 2)
      # data<-read.csv("/Users/hamid/Dropbox/My R codes/Bin-Hamid/Stock_Prob/aaa.csv")
      # Random_Forrest.train <- Boruta(TARGET~., data = data, doTrace = 2)
      #cat("The Random Forest Feature Selection is finished here \n")
      Random_Forrest_fs<-as.data.frame(Random_Forrest.train$finalDecision)
      names(Random_Forrest_fs)[1]<-paste("criteria")
      Random_Forrest_imp<-as.data.frame.table(subset(Random_Forrest_fs, Random_Forrest_fs[,1] == "Confirmed"))
      names(Random_Forrest_imp)[1]<-paste("variables")
      Random_Forrest_imp_tmp<-Random_Forrest_imp
      #checking to see if variables are from before 2014
      names(Random_Forrest_imp)[2]<-c("version")
      write.csv(Random_Forrest_imp,input_object$settings["Random_Forrest_Location","content"])
    }else{
      Random_Forrest_imp<-read.csv(input_object$settings["Random_Forrest_Location","content"])}
    input_object$Random_Forrest<-Random_Forrest_imp}
  return(input_object)}

#=============================================================
#==============Lasso Feature selection for multinomial TARGETS
#=============================================================
Lasso_Multinomial<-function(input_object){
  if(as.numeric(input_object$parameters["Multi_Bino","content"]=="Y")){
    if(as.numeric(input_object$parameters["FS_Method_2","content"])>0){
      if(input_object$parameters["Lasso_run","content"]=="N"){
        x<-model.matrix( ~ .-1,input_object$data[,1:ncol(input_object$data)-1])
        
        library(glmnet)
        y<-input_object$dataset$TARGET
        y<-as.numeric(y)
        fit1=glmnet(x,y,family="multinomial",type.multinomial="grouped", alpha = 1)
        
        nvariables<- data.frame(fit1$df)
        deviation<- data.frame(fit1$dev.ratio)
        lambda<-data.frame(fit1$lambda)
        
        Lasso<-cbind(nvariables,deviation,lambda)
        colnames(Lasso)<-c("variables","variability","lambda")
        Lasso$changes<-0
        Lasso$changes[1]<-Lasso$variability[1]
        for(i in 2:nrow(Lasso)){
          Lasso$changes[i]<-Lasso$variability[i]-Lasso$variability[i-1]
        }
        
        picked_lambda<-.0055
        coefs<-coef(fit1,s=picked_lambda)
        a<-coefs
        Lasso_vars_no<-data.frame(a@i)
        colnames(Lasso_vars_no)<-c("rank")
        Lasso_vars<-data.frame(a@Dimnames)
        colnames(Lasso_vars)<-c("variables","selected")
        Lasso_vars$selected<-NA
        
        for (i in 1:nrow(Lasso_vars_no)) {
          b<-Lasso_vars_no$rank[i]
          Lasso_vars$selected[b]<-1
        }
        Lasso_vars_cleaned<-Lasso_vars[-which(is.na(Lasso_vars$selected)),]
        
        Var_imp<-data.frame(VarsData_Phase[,1])
        Var_imp$selected<-NA
        colnames(Var_imp)<-c("variables","selected")
        for (i in 1:nrow(Var_imp)){
          for (j in 1:nrow(Lasso_vars_cleaned)) {
            if (grepl(Var_imp$variables[i],Lasso_vars_cleaned$variables[j])){
              if(str_count(Var_imp$variables[i],"_")==str_count(Lasso_vars_cleaned$variables[j],"_")){
                {Var_imp$selected[i]<-1}
              }
            }
          }
        }
        Lasso_imp_cleaned<-Var_imp[-which(is.na(Var_imp$selected)),]
        
        #checking to see if variables are from before 2014
        Lasso_imp_cleaned$version<-NA
        
        old_vars<-data.frame(colnames(input_object$dataset))
        colnames(old_vars)<-c("variables")
        Lasso_imp_cleaned$version<-as.character(Lasso_imp_cleaned$version)
        input_object$Lasso<-Lasso_imp_cleaned
        write.csv(Lasoo_imp_cleaned,input_object$settings["Lasso_location","content"])}
      else{input_object$Lasso<-read.csv(input_object$settings["Lasso_location","content"])}
      input_object$Lasso<-Lasoo_imp_cleaned
    }}
  return(input_object)
}    
#=============================================================
#==============Lasso Feature selection for Binomial TARGETS
#=============================================================
Lasso_Binomial<-function(input_object){
  if(input_object$parameters["Multi_Bino","content"]=="N"){
    if(as.numeric(input_object$parameters["FS_Method_2","content"])>0){
      if(input_object$parameters["Lasso_run","content"]=="N"){
        x<-model.matrix( ~ .-1,input_object$dataset[,1:ncol(input_object$dataset)-1])
        
        library(glmnet)
        y<-input_object$dataset$TARGET
        y<-as.numeric(y)
        fit1=glmnet(x,y,family="binomial" , alpha = 1)
        
        variables<- data.frame(fit1$df)
        deviation<- data.frame(fit1$dev.ratio)
        lambda<-data.frame(fit1$lambda)
        
        Lasso<-cbind(variables,deviation,lambda)
        colnames(Lasso)<-c("variables","variability","lambda")
        Lasso$changes<-0
        Lasso$changes[1]<-Lasso$variability[1]
        picked_lambda<-.02
        for(i in 2:nrow(Lasso)){
          Lasso$changes[i]<-Lasso$variability[i]-Lasso$variability[i-1]}
        
        avg_change<-(tail(Lasso$variability,1)-head(Lasso$variability,1))/nrow(Lasso)
        
        begin_range<-round(nrow(Lasso)*.05)
        end_range<-round(nrow(Lasso)*.95)
        avg_change<-mean(Lasso$variability[begin_range:end_range])
        
        for(i in 2:nrow(Lasso)){
          if(Lasso$changes[i]>avg_change){picked_lambda<-Lasso$lambda[i]} }
        
        plot(Lasso$variables,Lasso$variability)
        plot(Lasso$lambda,Lasso$variability)
        plot(Lasso$lambda,Lasso$variables)
        
        coefs<-coef(fit1,s=picked_lambda)
        a<-coefs
        Lasso_vars_no<-data.frame(a@i)
        colnames(Lasso_vars_no)<-c("rank")
        Lasso_vars<-data.frame(a@Dimnames)
        colnames(Lasso_vars)<-c("variables","selected")
        Lasso_vars$selected<-NA
        
        for (i in 1:nrow(Lasso_vars_no)) {
          b<-Lasso_vars_no$rank[i]
          Lasso_vars$selected[b]<-1
        }
        Lasso_vars_cleaned<-Lasso_vars[-which(is.na(Lasso_vars$selected)),]
        
        Var_imp<-data.frame(colnames(input_object$dataset))
        Var_imp$selected<-NA
        colnames(Var_imp)<-c("variables","selected")
        for (i in 1:nrow(Var_imp)){
          for (j in 1:nrow(Lasso_vars_cleaned)) {
            if (grepl(Var_imp$variables[i],Lasso_vars_cleaned$variables[j])){
              if(str_count(Var_imp$variables[i],"_")==str_count(Lasso_vars_cleaned$variables[j],"_")){
                {Var_imp$selected[i]<-1}
              }
            }
          }
        }
        
        Lasso_imp_cleaned<-Var_imp[-which(is.na(Var_imp$selected)),]
        Lasso_imp_cleaned$variables<-as.character(Lasso_imp_cleaned$variables)
        input_object$Lasso<-Lasso_imp_cleaned
        write.csv(Lasso_imp_cleaned,input_object$settings["Lasso_location","content"])}
      else{input_object$Lasso<-read.csv(input_object$settings["Lasso_location","content"])}
    }}
  return(input_object)}

#=============================================================
#==============Fast Feature selection
#=============================================================

FFS<-function(input_object){
  if(as.numeric(input_object$parameters["FS_Method_3","content"])>0){
    if(input_object$parameters["FCBF_run","content"]=="N"){
      library(Biocomb)
      disc<-"MDL"
      threshold=0
      attrs.nominal=numeric()
      FF_vars=select.fast.filter(input_object$dataset, disc.method=disc, threshold=threshold,
                                 attrs.nominal=attrs.nominal)
      
      FF_vars$Information.Gain<-NULL
      FF_vars$NumberFeature<-NULL
      names(FF_vars)<-"variables"
      input_object$FFS<-FF_vars
      write.csv(FF_vars,input_object$settings["Fast_Feature_Location","content"])}
    else{input_object$FFS<-read.csv(input_object$settings["Fast_Feature_Location","content"])}
  }
  return(input_object)}  

#============================================================================================
#==============Itself Feature Selection
#============================================================================================
itself<-function(input_object){
  if(as.numeric(input_object$parameters["FS_Method_4","content"]>0)){
    if(input_object$parameters["itself_run","content"]=="N"){
      data<-input_object$dataset
      data<-dummy_maker(data,1,ncol(data)-1)
      x<-data[ , !(names(data) %in% "TARGET")]
      TARGET<-as.factor(data$TARGET)
      #x<-pre_process(x)
      for(i in 1: length(levels(TARGET))){levels(TARGET)[i] <- paste("Level_",i,sep = "")}
      subsets <- c(1:ncol(x))
      
      caretFuncs$summary <- twoClassSummary
      ctrl <- rfeControl(functions=caretFuncs, 
                         number = 1,
                         returnResamp="final", verbose = TRUE)
      
      trainctrl <- trainControl(classProbs= TRUE,
                                summaryFunction = twoClassSummary)
      
      if(input_object$parameters["mac","content"]=="Y"){
        registerDoMC(cores = NULL)
        registerDoMC(cores = 4)}
      svm_Profile <- rfe(x, TARGET,
                         sizes=subsets,
                         rfeControl=ctrl,
                         method="svmRadial",
                         metric = "ROC",
                         trControl = trainctrl)
      if(input_object$parameters["mac","content"]=="Y"){
        registerDoMC(cores = NULL)
        registerDoMC(cores = 4)}
      log_Profile <- rfe(x, TARGET,
                         sizes=subsets,
                         rfeControl=ctrl,
                         method="glm",
                         metric="ROC",
                         trControl=trainctrl)
      if(input_object$parameters["mac","content"]=="Y"){
        registerDoMC(cores = NULL)
        registerDoMC(cores = 4)}
      rfeControl = rfeControl(functions = rfFuncs,
                              number = 1,
                              verbose = TRUE,
                              saveDetails = TRUE,
                              allowParallel = TRUE)
      CRT_Profile <- rfe(x, TARGET,
                         sizes=subsets,
                         rfeControl=rfeControl,
                         method="rpart1SE",
                         metric="ROC",
                         trControl=trainctrl)
      
      data_tan<-x
      for(i in 1:ncol(data_tan)){
        if(is.numeric(data_tan[,i])){
          data_tan[,i]<-discretize(data_tan[,i],method="interval",categories = 5)
        }
      }
      if(input_object$parameters["mac","content"]=="Y"){
        registerDoMC(cores = NULL)
        registerDoMC(cores = 4)}
      TAN_Profile <- rfe(data_tan, TARGET,
                         sizes=subsets,
                         rfeControl=rfeControl,
                         method="tan",
                         metric="ROC",
                         trControl=trainctrl,
                         tuneLength = NULL)
      if(input_object$parameters["mac","content"]=="Y"){
        registerDoMC(cores = NULL)
        registerDoMC(cores = 4)}
      NNET_Profile <- rfe(x, TARGET,
                          sizes=subsets,
                          rfeControl=ctrl,
                          method="nnet",
                          metric="ROC",
                          trControl=trainctrl)
      if(input_object$parameters["mac","content"]=="Y"){registerDoMC(cores = NULL)}
      
      vars<-as.data.frame(colnames(input_object$dataset))
      input_object$itself$svm<-var_finder(vars,as.data.frame(svm_Profile$optVariables))
      input_object$itself$log<-var_finder(vars,as.data.frame(log_Profile$optVariables))
      input_object$itself$crt<-var_finder(vars,as.data.frame(CRT_Profile$optVariables))
      input_object$itself$tan<-var_finder(vars,as.data.frame(TAN_Profile$optVariables))
      input_object$itself$nnet<-var_finder(vars,as.data.frame(NNET_Profile$optVariables))
      write.csv(input_object$itself$svm,input_object$settings["itself_svm_Location","content"])
      write.csv(input_object$itself$log,input_object$settings["itself_log_Location","content"])
      write.csv(input_object$itself$crt,input_object$settings["itself_CRT_Location","content"])
      write.csv(input_object$itself$tan,input_object$settings["itself_tan_Location","content"])
      write.csv(input_object$itself$nnet,input_object$settings["itself_NNET_Location","content"])
    }else{
      tttt<-read.csv(input_object$settings["itself_svm_Location","content"]);tttt$X<-NULL;input_object$itself$svm<-tttt
      tttt<-read.csv(input_object$settings["itself_log_Location","content"]);tttt$X<-NULL;input_object$itself$log<-tttt
      tttt<-read.csv(input_object$settings["itself_CRT_Location","content"]);tttt$X<-NULL;input_object$itself$crt<-tttt
      tttt<-read.csv(input_object$settings["itself_tan_Location","content"]);tttt$X<-NULL;input_object$itself$tan<-tttt
      tttt<-read.csv(input_object$settings["itself_NNET_Location","content"]);tttt$X<-NULL;input_object$itself$nnet<-tttt
    }
  }
  return(input_object)}

#=============================================================
#==============Consolidating the Features
#=============================================================  

consolidate<-function(input_object){
  if(input_object$parameters["itself_run","content"]=="N"){
    template<-data.frame(NA)
    names(template)<-"variables"
    
    RF<-template
    Lasso<-template
    FFS<-template
    itself<-template
    lit<-template
    itself_svm<-template
    itself_log<-template
    itself_crt<-template
    itself_tan<-template
    itself_nnet<-template
    
    if(!is.null(input_object$Random_Forrest)){
      if(as.numeric(input_object$parameters["FS_Method_1","content"])>0){RF<-as.data.frame(input_object$Random_Forrest$variables);names(RF)<-"variables"}}
    if(!is.null(input_object$Lasso)){
      if(as.numeric(input_object$parameters["FS_Method_2","content"])>0){Lasso<-as.data.frame(input_object$Lasso$variables);names(Lasso)<-"variables"}}
    if(!is.null(input_object$FFS)){
      if(as.numeric(input_object$parameters["FS_Method_3","content"])>0){FFS<-as.data.frame(input_object$FFS$variables);names(FFS)<-"variables"}}
    
    if(as.numeric(input_object$parameters["FS_Method_4","content"])>0){itself_all<-input_object$itself
    
    if(!is.null(input_object$itself$svm)){itself_svm<-itself_all$svm}
    if(!is.null(input_object$itself$log)){itself_log<-itself_all$log}
    if(!is.null(input_object$itself$crt)){itself_crt<-itself_all$crt}
    if(!is.null(input_object$itself$tan)){itself_tan<-itself_all$tan}
    if(!is.null(input_object$itself$nnet)){itself_nnet<-itself_all$nnet}
    
    itself<-rbind(itself_svm,itself_log,itself_crt,itself_tan,itself_nnet)
    itself<-as.data.frame(itself[!duplicated(itself$variables), ])
    names(itself)<-"variables"
    } 
    if(!is.null(input_object$lit)){lit<-input_object$lit}
    vars<-rbind(RF,Lasso,FFS,itself,lit)
    vars<-na.omit(vars)
    vars<-as.data.frame(vars[!duplicated(vars$variables), ])
    names(vars)<-"variables"
    
    input_object$itself$consolidated<-vars
    write.csv(input_object$itself$consolidated,input_object$settings["consolidate_Location","content"])}
  else{input_object$itself$consolidated<-read.csv(input_object$settings["consolidate_Location","content"])}
  return(input_object)
}

#=============================================================
#==============Automated Data Preparation
#=============================================================
ADP<-function(input_object){
  data<-input_object$dataset
  data$TARGET<-NULL
  #dropping variables with too many categories
  for(i in 1:ncol(input_object$dataset)){
    if(is.factor(input_object$dataset[,i])){
      if(nlevels(input_object$dataset[,i])>10){data[,i]<-NULL}}}
  
  #Making dummy variables
  data$TARGET<-input_object$dataset$TARGET
  data$TARGET<-as.factor(data$TARGET)
  aaaa<-dummyVars(TARGET ~ ., data = data)
  bbb<-as.data.frame(predict(aaaa, newdata = input_object$dataset))
  data<-bbb
  #End of dummy making
  
  #Excluding near zero variances
  nzv <-nearZeroVar(data, saveMetrics = TRUE)
  nzv<-nzv$nzv
  if(TRUE %in% nzv){data <- data[, -nzv]}
  #End of zero variance variables
  
  #Excluding highly correlated columns
  ddd<-data
  ddd$TARGET<-NULL
  var_type<-as.data.frame(names(ddd))
  var_type$numeric<-NA
  
  for(i in 1:ncol(ddd)){if(is.numeric(ddd[,i])){
    var_type$numeric[i]<-"yes"}}
  numerics<-var_type[!is.na(var_type$numeric),]
  non_numerics<-var_type[is.na(var_type$numeric),]
  
  numeric_vars<-numerics[,1]
  non_numeric_vars<-non_numerics[,1]
  
  data_numerics<-ddd[, which(names(ddd) %in% numeric_vars)]
  
  data_non_numerics<-ddd[, which(names(ddd) %in% non_numeric_vars)]
  if(nrow(non_numerics)==1){data_non_numerics<-as.data.frame(data_non_numerics)
  names(data_non_numerics)<-as.character(non_numeric_vars)}
  
  descrCor <- cor(data_numerics)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
  if(length(highlyCorDescr)>1){
    data_numerics <- data_numerics[, -highlyCorDescr]}
  data<-cbind(data_non_numerics,data_numerics)
  #End of highly correlated columns
  
  #Excluding Linear dependencies in columns
  comboInfo <- findLinearCombos(data)
  if(!is.null(comboInfo$remove)){data<-data[, -comboInfo$remove]}
  #End of Linear dependencies
  
  #Scaling and Centering data
  preProcValues <- preProcess(data, method = c("center", "scale"))
  data<- predict(preProcValues, data)
  #End of Scaling and Centering
  
  # #PCA
  #data_pca<-prcomp(data)
  #data_pca<-data_pca$x
  # data<-data_pca
  # #End of PCA
  data$TARGET<-input_object$dataset$TARGET
  input_object$dataset<-data
  return(input_object)
}

#=============================================================
#==============Multinomial prediction
#=============================================================  
multi_pred<-function(input_object){
  if(input_object$parameters["Multi_Bino","content"]=="Y"){
    category_no<-as.numeric(input_object$parameters["category_no","content"])
    
    if(category_no>=1){levels(data$TARGET)[1] <- "One"}
    if(category_no>=2){levels(data$TARGET)[2] <- "Two"}
    if(category_no>=3){levels(data$TARGET)[3] <- "Three"}
    if(category_no>=4){levels(data$TARGET)[4] <- "Four"}
    if(category_no==5){levels(data$TARGET)[5] <- "Five"}
    
    control_<- trainControl(method="cv",number = input_object$parameters["n_folds","content"] ,savePredictions = TRUE,
                            verboseIter = FALSE,returnResamp = "all",classProbs = TRUE)
    
    mod_log <- train(TARGET~.,  data=input_object$dataset, method="multinom", 
                     trControl = control_, tuneLength = 10,tuneGrid = expand.grid(decay=0.001))
    
    if(input_object$parameters["tuning","content"]=="Y"){
      control_tune_svm <- trainControl(method="repeatedcv", number=5, repeats=5)
      grid <- expand.grid(C=seq(.2,5,.1), sigma=seq(.000005,.0001,.00001))
      model_tune_svm <- train(TARGET~., data=data, method="svmRadial", trControl=control_tune_svm, tuneGrid=grid)
      svm_sigma<-as.numeric(model_tune_svm$bestTune$sigma)
      svm_cost<-as.numeric(model_tune_svm$bestTune$C)}else{
        svm_cost<-.5
        svm_sigma<-.0001
      }
    
    
    mod_svm<-train(TARGET~.,  data=data, method="svmRadial", 
                   trControl = control_,  tuneGrid=expand.grid(C=svm_cost, sigma=svm_sigma),
                   tuneLength = 10)
    
    mod_CRT <- train(TARGET~.,  data=data, method="rpart1SE", 
                     trControl = control_, tuneLength = 10)
    
    mod_NNET<-train(TARGET~.,  data=data, method="nnet", 
                    trControl = control_,  tuneGrid=expand.grid(size=6, decay=0.1),
                    tuneLength = 10)
  } 
  
  return(input_object) 
}

#=============================================================
#==============Binomial prediction
#=============================================================  
Bino_pred<-function(input_object){
  if(input_object$parameters["Multi_Bino","content"]=="N"){
    data<-input_object$dataset
    balanc_proc<-input_object$parameters["balanc_proc","content"]
    n_folds<-as.numeric(input_object$parameters["n_folds","content"])
    B_alg<-as.numeric(input_object$parameters["B_alg","content"])
    analysis_folder<-input_object$settings["analysis_folder","content"]
    method_fs<-input_object$settings["method_fs","content"]
    is_multi<-input_object$settings["is_multi","content"]
    is_bal<-input_object$settings["is_bal","content"]
    log_imp_folds_file<-input_object$settings["log_imp_folds_file","content"]
    svm_imp_folds_file<-input_object$settings["svm_imp_folds_file","content"]
    CRT_imp_folds_file<-input_object$settings["CRT_imp_folds_file","content"]
    NNET_imp_folds_file<-input_object$settings["NNET_imp_folds_file","content"]
    tan_imp_folds_file<-input_object$settings["tan_imp_folds_file","content"]
    VarsData_Phase<-input_object$VarsData_Phase
    log_perf_folds_file<-input_object$settings["log_perf_folds_file","content"]
    svm_perf_folds_file<-input_object$settings["svm_perf_folds_file","content"]
    CRT_perf_folds_file<-input_object$settings["CRT_perf_folds_file","content"]
    NNET_perf_folds_file<-input_object$settings["NNET_perf_folds_file","content"]
    tan_perf_folds_file<-input_object$settings["tan_perf_folds_file","content"]
    
    Avg_log_perf_folds_file<-input_object$settings["Avg_log_perf_folds_file","content"]
    Avg_svm_perf_folds_file<-input_object$settings["Avg_svm_perf_folds_file","content"]
    Avg_CRT_perf_folds_file<-input_object$settings["Avg_CRT_perf_folds_file","content"]
    Avg_NNET_perf_folds_file<-input_object$settings["Avg_NNET_perf_folds_file","content"]
    Avg_tan_perf_folds_file<-input_object$settings["Avg_tan_perf_folds_file","content"]
    H_por_low<-as.numeric(input_object$parameters["H_por_low","content"])
    H_por_high<-as.numeric(input_object$parameters["H_por_high","content"])
    
    conso_feat<-input_object$parameters["conso_feat","content"]
    
    levels(data$TARGET)[1] <- "One"
    levels(data$TARGET)[2] <- "Two"
    
    #____________________making the n folds_____________________________________
    
    Train_Two_No<-length(which(data$TARGET=="Two"))
    Train_One_No<-length(which(data$TARGET=="One"))
    
    # Create the training and testing data sets
    
    kept_rows_Train_Two<-floor(Train_Two_No/n_folds)*n_folds
    kept_rows_Train_One<-floor(Train_One_No/n_folds)*n_folds
    
    
    Train_Two <- data[ which(data$TARGET=="Two"), ]
    Train_One <- data[ which(data$TARGET=="One"), ]
    
    #shuffling
    Train_Two<-Train_Two[sample(nrow(Train_Two),kept_rows_Train_Two),]
    Train_One<-Train_One[sample(nrow(Train_One),kept_rows_Train_One),]
    
    #naming the folds ID
    folds_Train_Two <- cut(seq(1,nrow(Train_Two)),breaks=n_folds,labels=FALSE)
    folds_Train_One <- cut(seq(1,nrow(Train_One)),breaks=n_folds,labels=FALSE)
    
    Train_Two$folds<-folds_Train_Two
    Train_One$folds<-folds_Train_One
    
    Train<-rbind(Train_Two,Train_One)
    
    ###### end of modeling for ten folds
    
    #____________setup ensemble modeling ____
    
    control_<- trainControl(method = "none",  savePredictions = TRUE, 
                            verboseIter = FALSE,returnResamp = "all",classProbs = TRUE, summaryFunction = twoClassSummary)
    
    rows_no<-floor(nrow(Train)/n_folds)
    
    resul_log_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_log_raw<-as.data.frame(resul_log_raw)
    
    resul_log_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_log_prob<-as.data.frame(resul_log_prob)
    
    resul_svm_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_svm_raw<-as.data.frame(resul_svm_raw)
    
    resul_svm_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_svm_prob<-as.data.frame(resul_svm_prob)
    
    resul_CRT_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_CRT_raw<-as.data.frame(resul_CRT_raw)
    
    resul_CRT_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_CRT_prob<-as.data.frame(resul_CRT_prob)
    
    resul_NNET_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_NNET_raw<-as.data.frame(resul_NNET_raw)
    
    resul_NNET_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_NNET_prob<-as.data.frame(resul_NNET_prob)
    
    resul_tan_raw<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_tan_raw<-as.data.frame(resul_tan_raw)
    
    resul_tan_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_tan_prob<-as.data.frame(resul_tan_prob)   
    
    resul_fusion_prob<-matrix(0, ncol = n_folds, nrow = rows_no)
    resul_fusion_prob<-as.data.frame(resul_fusion_prob)
    
    resul_log_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
    resul_log_imp<-as.data.frame(resul_log_imp)
    names(resul_log_imp) <- paste("fold", 1:n_folds, sep = "")
    
    resul_svm_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
    resul_svm_imp<-as.data.frame(resul_svm_imp)
    names(resul_svm_imp) <- paste("fold", 1:n_folds, sep = "")
    
    resul_CRT_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
    resul_CRT_imp<-as.data.frame(resul_CRT_imp) 
    names(resul_CRT_imp) <- paste("fold", 1:n_folds, sep = "")
    
    resul_NNET_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
    resul_NNET_imp<-as.data.frame(resul_NNET_imp) 
    names(resul_NNET_imp) <- paste("fold", 1:n_folds, sep = "")
    
    resul_tan_imp<-matrix(0, ncol = n_folds, nrow = ncol(data)-1)
    resul_tan_imp<-as.data.frame(resul_tan_imp) 
    names(resul_tan_imp) <- paste("fold", 1:n_folds, sep = "")
    
    rownames(resul_log_imp)<-colnames(data[,1:ncol(data)-1])
    resul_log_imp$variables<-colnames(data[,1:ncol(data)-1])
    rownames(resul_svm_imp)<-colnames(data[,1:ncol(data)-1])
    resul_svm_imp$variables<-colnames(data[,1:ncol(data)-1])
    rownames(resul_CRT_imp)<-colnames(data[,1:ncol(data)-1])
    resul_CRT_imp$variables<-colnames(data[,1:ncol(data)-1])
    rownames(resul_NNET_imp)<-colnames(data[,1:ncol(data)-1])
    resul_NNET_imp$variables<-colnames(data[,1:ncol(data)-1])
    rownames(resul_tan_imp)<-colnames(data[,1:ncol(data)-1])
    resul_tan_imp$variables<-colnames(data[,1:ncol(data)-1])
    
    if(conso_feat=="Y"){
      consolidated_vars<-input_object$itself$consolidated
      vars<-as.character(consolidated_vars$variables)
      if(input_object$parameters["lit_run","content"]!="K"){
        lit_vars<-input_object$lit
        vars<-rbind(consolidated_vars,lit_vars)
        vars<-as.data.frame(vars[!duplicated(vars$variables), ])
        names(vars)<-"variables"
        vars<-na.omit(vars)
        vars<-as.character(vars$variables)}
      tan_vars<-vars
      formula_format<-paste("TARGET ~ ",paste(vars, collapse="+"),sep = "")
      formula<-as.formula(formula_format)}
    
    if(conso_feat=="N"){
      vars_log<-input_object$itself$log
      vars_svm<-input_object$itself$svm
      vars_CRT<-input_object$itself$crt
      vars_NNET<-input_object$itself$nnet
      vars_tan<-input_object$itself$tan
      
      log_vars<-as.character(vars_log$variables)
      svm_vars<-as.character(vars_svm$variables)
      CRT_vars<-as.character(vars_CRT$variables) 
      NNET_vars<-as.character(vars_NNET$variables)
      tan_vars<-as.character(vars_tan$variables)
      
      if(input_object$parameters["lit_run","content"]!="K"){
        lit_vars<-input_object$lit
        
        log_vars<-rbind(vars_log,lit_vars)
        log_vars<-as.data.frame(log_vars[!duplicated(log_vars$variables), ])
        names(log_vars)<-"variables"
        log_vars<-na.omit(log_vars)
        log_vars<-as.character(log_vars$variables)
        
        svm_vars<-rbind(vars_svm,lit_vars)
        svm_vars<-as.data.frame(svm_vars[!duplicated(svm_vars$variables), ])
        names(svm_vars)<-"variables"
        svm_vars<-na.omit(svm_vars)
        svm_vars<-as.character(svm_vars$variables)
        
        CRT_vars<-rbind(vars_CRT,lit_vars)
        CRT_vars<-as.data.frame(CRT_vars[!duplicated(CRT_vars$variables), ])
        names(CRT_vars)<-"variables"
        CRT_vars<-na.omit(CRT_vars)
        CRT_vars<-as.character(CRT_vars$variables)
        
        NNET_vars<-rbind(vars_NNET,lit_vars)
        NNET_vars<-as.data.frame(NNET_vars[!duplicated(NNET_vars$variables), ])
        names(NNET_vars)<-"variables"
        NNET_vars<-na.omit(NNET_vars)
        NNET_vars<-as.character(NNET_vars$variables)
        
        tan_vars<-rbind(vars_tan,lit_vars)
        tan_vars<-as.data.frame(tan_vars[!duplicated(tan_vars$variables), ])
        names(tan_vars)<-"variables"
        tan_vars<-na.omit(tan_vars)
        tan_vars<-as.character(tan_vars$variables)}
      
      formula_format_log<-paste("TARGET ~ ",paste(log_vars, collapse="+"),sep = "")
      formula_format_svm<-paste("TARGET ~ ",paste(svm_vars, collapse="+"),sep = "")
      formula_format_CRT<-paste("TARGET ~ ",paste(CRT_vars, collapse="+"),sep = "")
      formula_format_NNET<-paste("TARGET ~ ",paste(NNET_vars, collapse="+"),sep = "")
      formula_format_tan<-paste("TARGET ~ ",paste(tan_vars, collapse="+"),sep = "")
      formula_log<-as.formula(formula_format_log)
      formula_svm<-as.formula(formula_format_svm)
      formula_CRT<-as.formula(formula_format_CRT)
      formula_NNET<-as.formula(formula_format_NNET)
      formula_tan<-as.formula(formula_format_tan)
    }
    
    #_______model training and testing_____
    library(stringr)
    i<-3
    ##model training__
    for ( i in 1: n_folds){
      test_data<-Train[Train$folds == i,]
      train_data<-Train[Train$folds != i,]
      
      tt<-as.data.frame(table(train_data$TARGET))
      table(train_data$TARGET)
      common<-tt$Freq[1]
      rare<-tt$Freq[2]
      k<-1
      if(common<rare){
        temp_<-common
        common<-rare
        rare<-temp_
      }
      
      if(B_alg==1){
        H_por_low<-1
        H_por_high<-1
        H_por_low<-(1/H_por_low)
        H_por_high<-H_por_high*100 
        perc_over<-100*(common-rare)/(rare*H_por_low)*k
        perc_under<-100*(1/perc_over)*common
        train_data<-SMOTE(TARGET ~ ., train_data, perc.over = perc_over,perc.under = (((perc_over/100)+1)/(perc_over/100))*H_por_high)
        table(train_data$TARGET)
      }
      if(B_alg==2){
        train_data2<-ovun.sample(TARGET ~ .,data=train_data,method="under")
        train_data<-train_data2$data
        table(train_data$TARGET)
      }
      if(B_alg==3){
        H_por_low_<-(1/H_por_low)
        H_por_high_<-H_por_high*100 
        perc_over<-100*(common-rare)/(rare*(((common-rare)/(rare*H_por_low_))))
        
        train_data<-SMOTE(TARGET ~ ., train_data, perc.over = perc_over,perc.under = (((perc_over/100)+1)/(perc_over/100))*H_por_high_)
        
        table(train_data$TARGET)
      }
      
      balanced_data_train<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_multi,"_","_",is_bal,"_","fold-",i,"_","balanced-train.csv"),collapse = "")
      balanced_data_test<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_multi,"_","_",is_bal,"_","fold-",i,"_","balanced-test.csv"),collapse = "")
      write.csv(train_data,balanced_data_train)
      write.csv(test_data,balanced_data_test) 
      
      # keeping the record of Target distribution in each fold
      dis_train<-paste(c("K_",i),collapse = "")
      input_object$folds_dist[[dis_train]]<-table(train_data$TARGET)
      
      # Recording train and test folds
      name_train<-paste(c("train_fold_",i),collapse = "")
      input_object[[name_train]]<-train_data
      
      name_test<-paste(c("test_fold_",i),collapse = "")
      input_object[[name_test]]<-test_data
      
      # Recording address of train and test folds
      name_train_add<-paste(c("train_fold_add_",i),collapse = "")
      input_object[[name_train_add]]<-balanced_data_train
      
      name_test_add<-paste(c("test_fold_add_",i),collapse = "")
      input_object[[name_test_add]]<-balanced_data_test
      
      # train_data and test data distribution
      name_dist<-paste(c("fold_dist_",i),collapse = "")
      input_object[[name_dist]]<-table(train_data$TARGET)
      
      train_data$folds<-NULL
      
      if(conso_feat=="N"){formula<-formula_log}
      mod_log <-train(formula,  data=train_data, method="glm", family="binomial",
                      trControl = control_, tuneLength = 10, metric="ROC")
      
      if(conso_feat=="N"){formula<-formula_CRT}
      mod_CRT <-train(formula,  data=train_data, method="rpart1SE", 
                      trControl = control_, tuneLength = 10, metric="ROC")
      if(input_object$parameters["tuning","content"]=="Y"){
        control_tune_svm <- trainControl(method="repeatedcv", number=5, repeats=5)
        grid <- expand.grid(C=seq(.2,5,.1), sigma=seq(.000005,.0001,.00001))
        model_tune_svm <- train(TARGET~., data=data, method="svmRadial", trControl=control_tune_svm, tuneGrid=grid)
        svm_sigma<-as.numeric(model_tune_svm$bestTune$sigma)
        svm_cost<-as.numeric(model_tune_svm$bestTune$C)}else{
          svm_cost<-.5
          svm_sigma<-.0001
        }
      
      if(conso_feat=="N"){formula<-formula_svm}
      mod_svm<-train(formula,  data=train_data, method="svmRadial", family="binomial",
                     trControl = control_,  tuneGrid=expand.grid(C=svm_cost, sigma=svm_sigma),
                     tuneLength = 10)
      
      if(conso_feat=="N"){formula<-formula_NNET}
      mod_NNET <-train(formula,  data=train_data, method="nnet", family="binomial",
                       trControl = control_,  tuneGrid=expand.grid(size=5, decay=0.1), MaxNWts=20000,
                       tuneLength = 10) 
      ##===making data ready for TAN
      # disceretizing train and test
      data_tan_train<-train_data
      y_tr<-data_tan_train$TARGET
      data_tan_train$TARGET<-NULL
      data_tan_train$folds<-NULL
      data_tan_train$type<-"tr"
      
      data_tan_test<-test_data
      y_ts<-data_tan_test$TARGET
      data_tan_test$TARGET<-NULL
      data_tan_test$folds<-NULL
      data_tan_test$type<-"ts"
      data_tan<-rbind(data_tan_train,data_tan_test)
      
      
      for(j in 1:(ncol(data_tan)-1)){
        if(is.numeric(data_tan_train[,j])){
          data_tan[,j]<-discretize(data_tan[,j],method="interval",categories = 10)
        }
      }
      
      data_tan_train<-data_tan[which(data_tan$type=="tr"),]
      data_tan_test<-data_tan[which(data_tan$type=="ts"),]
      data_tan_train$type<-NULL
      data_tan_test$type<-NULL
      
      data_tan_train<-data_tan_train[, which(names(data_tan_train) %in% tan_vars)]
      mod_tan <- train(data_tan_train, y_tr, method="tan", 
                       trControl = control_, tuneGrid = expand.grid(score= "aic", smooth=0.5), 
                       tuneLength = 10)
      VarsData_Phase<-input_object$VarsData_Phase
      resul_log_imp<-matrix_filler(i,resul_log_imp,model_imp_exc(mod_log,VarsData_Phase),"variables")
      resul_svm_imp<-matrix_filler(i,resul_svm_imp,model_imp_exc(mod_svm,VarsData_Phase),"variables")  
      resul_CRT_imp<-matrix_filler(i,resul_CRT_imp,model_imp_exc(mod_CRT,VarsData_Phase),"variables")
      resul_NNET_imp<-matrix_filler(i,resul_NNET_imp,model_imp_exc(mod_NNET,VarsData_Phase),"variables")
      resul_tan_imp<-matrix_filler(i,resul_tan_imp,model_imp_exc(mod_tan,VarsData_Phase),"variables")
      
      ##model testing___
      resul_log_raw[,i]<-predict(mod_log, newdata=test_data, type="raw")
      resul_log_prob[,i]<-predict(mod_log, newdata=test_data, type="prob")
      
      resul_svm_raw[,i]<-predict(mod_svm, newdata=test_data, type="raw")
      resul_svm_prob[,i]<-predict(mod_svm, newdata=test_data, type="prob")
      
      
      resul_CRT_raw[,i]<-predict(mod_CRT, newdata=test_data, type="raw")
      resul_CRT_prob[,i]<-predict(mod_CRT, newdata=test_data, type="prob")
      
      resul_NNET_raw[,i]<-predict(mod_NNET, newdata=test_data, type="raw")
      resul_NNET_prob[,i]<-predict(mod_NNET, newdata=test_data, type="prob")    
      
      resul_tan_raw[,i]<-predict(mod_tan, newdata=data_tan_test, type="raw")
      resul_tan_prob[,i]<-predict(mod_tan, newdata=data_tan_test, type="prob") 
      
    }
    write.csv(resul_log_imp,log_imp_folds_file)
    write.csv(resul_svm_imp,svm_imp_folds_file)
    write.csv(resul_CRT_imp,CRT_imp_folds_file)
    write.csv(resul_NNET_imp,NNET_imp_folds_file)
    write.csv(resul_tan_imp,NNET_imp_folds_file)
    
    resul_fusion_prob<-(resul_log_prob+resul_svm_prob+resul_CRT_prob+resul_NNET_prob)/4
    
    #___________Specifying Tables for YES and NO probabilities_____________
    
    resul_log_prob_YES<-1-resul_log_prob
    resul_svm_prob_YES<-1-resul_svm_prob
    resul_CRT_prob_YES<-1-resul_CRT_prob
    resul_NNET_prob_YES<-1-resul_NNET_prob    
    resul_tan_prob_YES<-1-resul_tan_prob 
    
    resul_fusion_prob_YES<-((resul_log_prob_YES+resul_svm_prob_YES+resul_CRT_prob_YES+resul_NNET_prob_YES)/4)
    
    
    #____________________setting up dataframes for recording performance of models
    
    performance_log<-matrix(0, ncol = n_folds, nrow = 4)
    performance_log<-as.data.frame(performance_log)
    
    performance_svm<-matrix(0, ncol = n_folds, nrow = 4)
    performance_svm<-as.data.frame(performance_svm)
    
    performance_CRT<-matrix(0, ncol = n_folds, nrow = 4)
    performance_CRT<-as.data.frame(performance_CRT)
    
    performance_NNET<-matrix(0, ncol = n_folds, nrow = 4)
    performance_NNET<-as.data.frame(performance_NNET) 
    
    performance_tan<-matrix(0, ncol = n_folds, nrow = 4)
    performance_tan<-as.data.frame(performance_tan) 
    
    performance_fusion<-matrix(0, ncol = n_folds, nrow = 4)
    performance_fusion<-as.data.frame(performance_fusion)
    
    row.names(performance_log) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_log)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
    
    row.names(performance_svm) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_svm)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
    
    row.names(performance_CRT) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_CRT)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
    
    row.names(performance_NNET) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_NNET)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")  
    
    row.names(performance_tan) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_tan)[1:n_folds] <- paste("Fold", 1:n_folds, sep="") 
    
    row.names(performance_fusion) <- c("ROC","ACCURACY","SENSITIVITY","SPECIFICITY")
    names(performance_fusion)[1:n_folds] <- paste("Fold", 1:n_folds, sep="")
    
    #______a change just for calculating accuracy
    
    levels(Train$TARGET)[1] <- 0
    levels(Train$TARGET)[2]  <- 1
    
    #_________________________calculating the performance of the models_____________
    i<-3
    library(AUC)
    for (i in 1:n_folds){
      
      temp<-Train[Train$folds == i,]
      
      levels(temp$TARGET)[1] <- "One"
      levels(temp$TARGET)[2]  <- "Two"
      test_table<-temp$TARGET
      
      performance_log[1,i]<-AUC:: auc(roc(resul_log_prob_YES[,i],test_table))
      performance_svm[1,i]<-AUC:: auc(roc(resul_svm_prob_YES[,i],test_table))
      performance_CRT[1,i]<-AUC:: auc(roc(resul_CRT_prob_YES[,i],test_table))
      performance_NNET[1,i]<-AUC:: auc(roc(resul_NNET_prob_YES[,i],test_table))  
      performance_tan[1,i]<-AUC:: auc(roc(resul_tan_prob_YES[,i],test_table))
      performance_fusion[1,i]<-AUC:: auc(roc(resul_fusion_prob[,i],test_table))
      
      performance_log[3,i]<-caret:: sensitivity(resul_log_raw[,i],test_table)
      performance_svm[3,i]<-caret:: sensitivity(resul_svm_raw[,i],test_table)
      performance_CRT[3,i]<-caret:: sensitivity(resul_CRT_raw[,i],test_table)
      performance_NNET[3,i]<-caret:: sensitivity(resul_NNET_raw[,i],test_table)
      performance_tan[3,i]<-caret:: sensitivity(resul_tan_raw[,i],test_table)
      
      performance_log[4,i]<-caret:: specificity(resul_log_raw[,i],test_table)
      performance_svm[4,i]<-caret:: specificity(resul_svm_raw[,i],test_table)
      performance_CRT[4,i]<-caret:: specificity(resul_CRT_raw[,i],test_table)
      performance_NNET[4,i]<-caret:: specificity(resul_NNET_raw[,i],test_table)
      performance_tan[4,i]<-caret:: specificity(resul_tan_raw[,i],test_table)
      
      performance_log[2,i]<-(as.data.frame(confusionMatrix(resul_log_raw[,i],test_table)$overall))[1,]
      performance_svm[2,i]<-(as.data.frame(confusionMatrix(resul_svm_raw[,i],test_table)$overall))[1,]
      performance_CRT[2,i]<-(as.data.frame(confusionMatrix(resul_CRT_raw[,i],test_table)$overall))[1,]
      performance_NNET[2,i]<-(as.data.frame(confusionMatrix(resul_NNET_raw[,i],test_table)$overall))[1,]
      performance_tan[2,i]<-(as.data.frame(confusionMatrix(resul_tan_raw[,i],test_table)$overall))[1,]
      
    }
    for(i in 1:nrow(performance_log)){
      performance_log$avg[i]<-(sum(performance_log[i,1:n_folds])/n_folds)
      performance_svm$avg[i]<-(sum(performance_svm[i,1:n_folds])/n_folds)
      performance_CRT$avg[i]<-(sum(performance_CRT[i,1:n_folds])/n_folds)
      performance_NNET$avg[i]<-(sum(performance_NNET[i,1:n_folds])/n_folds)
      performance_tan$avg[i]<-(sum(performance_tan[i,1:n_folds])/n_folds)
    }
    
    write.csv(performance_log,log_perf_folds_file)
    write.csv(performance_svm,svm_perf_folds_file)
    write.csv(performance_CRT,CRT_perf_folds_file)
    write.csv(performance_NNET,NNET_perf_folds_file)
    write.csv(performance_tan,tan_perf_folds_file)
    
    #______This for finding weighte average for importance of variables based on AUC (ROC)    
    
    resul_log_imp_tmp<-resul_log_imp[,1:n_folds]
    resul_log_imp_tmp[]<-NA
    resul_svm_imp_tmp<-resul_svm_imp[,1:n_folds]
    resul_svm_imp_tmp[]<-NA
    resul_CRT_imp_tmp<-resul_CRT_imp[,1:n_folds]
    resul_CRT_imp_tmp[]<-NA
    resul_NNET_imp_tmp<-resul_NNET_imp[,1:n_folds]
    resul_NNET_imp_tmp[]<-NA
    resul_tan_imp_tmp<-resul_tan_imp[,1:n_folds]
    resul_tan_imp_tmp[]<-NA
    
    for(i in 1:n_folds){
      resul_log_imp_tmp[,i]<-resul_log_imp[,i]*performance_log[1,i]
      resul_svm_imp_tmp[,i]<-resul_svm_imp[,i]*performance_svm[1,i]
      resul_CRT_imp_tmp[,i]<-resul_CRT_imp[,i]*performance_CRT[1,i]
      resul_NNET_imp_tmp[,i]<-resul_NNET_imp[,i]*performance_NNET[1,i]
      resul_tan_imp_tmp[,i]<-resul_tan_imp[,i]*performance_tan[1,i]
    }
    for(i in 1:nrow(resul_log_imp_tmp)){
      resul_log_imp_tmp$weigh_avg[i]<-(sum(resul_log_imp_tmp[i,1:n_folds]))/sum(performance_log[1,1:n_folds])
      resul_svm_imp_tmp$weigh_avg[i]<-(sum(resul_svm_imp_tmp[i,1:n_folds]))/sum(performance_svm[1,1:n_folds])
      resul_CRT_imp_tmp$weigh_avg[i]<-(sum(resul_CRT_imp_tmp[i,1:n_folds]))/sum(performance_CRT[1,1:n_folds])
      resul_NNET_imp_tmp$weigh_avg[i]<-(sum(resul_NNET_imp_tmp[i,1:n_folds]))/sum(performance_NNET[1,1:n_folds])
      resul_tan_imp_tmp$weigh_avg[i]<-(sum(resul_tan_imp_tmp[i,1:n_folds]))/sum(performance_tan[1,1:n_folds])
    }
    resul_log_imp$Weigh_AUC_AVG<-resul_log_imp_tmp$weigh_avg
    resul_svm_imp$Weigh_AUC_AVG<-resul_svm_imp_tmp$weigh_avg
    resul_CRT_imp$Weigh_AUC_AVG<-resul_CRT_imp_tmp$weigh_avg 
    resul_NNET_imp$Weigh_AUC_AVG<-resul_NNET_imp_tmp$weigh_avg
    resul_tan_imp$Weigh_AUC_AVG<-resul_tan_imp_tmp$weigh_avg
    resul_log_imp_tmp<-NULL
    resul_svm_imp_tmp<-NULL
    resul_CRT_imp_tmp<-NULL
    resul_NNET_imp_tmp<-NULL
    resul_tan_imp_tmp<-NULL
    
    write.csv(resul_log_imp,Avg_log_perf_folds_file)
    write.csv(resul_svm_imp,Avg_svm_perf_folds_file)
    write.csv(resul_CRT_imp,Avg_CRT_perf_folds_file)
    write.csv(resul_NNET_imp,Avg_NNET_perf_folds_file)
    write.csv(resul_tan_imp,Avg_tan_perf_folds_file)
    
    input_object$performance$performance_log<-performance_log
    input_object$performance$performance_svm<-performance_svm
    input_object$performance$performance_CRT<-performance_CRT
    input_object$performance$performance_NNET<-performance_NNET
    input_object$performance$performance_tan<-performance_tan
    input_object$svm_cost<-svm_cost
    input_object$svm_sigma<-svm_sigma
    
    #______end of the weighted average module_______    
    
  }
  return(input_object)
}

#=============================================================
#==============Reporting Module
#============================================================= 

# Start writing to a Report file
Report<-function(input_object){
  balanc_proc<-input_object$parameters["balanc_proc","content"]
  Method_output<-
    method_fs<-input_object$settings["method_fs","content"]
  n_folds<-input_object$parameters["n_folds","content"]
  is_bal<-input_object$settings["is_bal","content"]
  is_multi<-input_object$settings["is_multi","content"]
  analysis_folder<-input_object$settings["analysis_folder","content"]
  H_por_low<-as.numeric(input_object$parameters["H_por_low","content"])
  H_por_high<-as.numeric(input_object$parameters["H_por_high","content"])
  data<-input_object$dataset
  
  sink(input_object$settings["Out_put_file","content"])
  cat("The feature selection method is: ","\n")
  cat(input_object$settings["FS_Method_output","content"],"\n")
  cat(paste(c("Type of the dependant variable is: ",input_object$settings["Method_output","content"]),collapse = ""),"\n")
  cat(paste(c("Number of folds is: ",n_folds),collapse = ""),"\n")
  if(balanc_proc=="Y"){
    cat("Sample balancing is used","\n")
    cat("The Sample balancing algorithm is: ",is_bal,"\n")
    
    if(input_object$parameters["B_alg","content"]==3){
      cat("Proportion of differences that you want to increase the minority is : ",H_por_low,"\n")
      cat("Multiplication for the size of minority as the size of majority : ",H_por_high,"\n")
    }
    
  }else{cat("Sample balancing is not used","\n")}
  cat("=====================================\n")
  cat("The distribution of data without Sample Balancing is:","\n")
  cat(table(data$TARGET),"\n")
  
  cat("The distribution of data with Sample Balancing in each loop is:","\n")
  for(i in 1:n_folds){
    cat(paste(c("fold ",i,":"),collapse = ""),"\n")
    dis_train<-paste(c("K_",i),collapse = "")
    cat(input_object$folds_dist[[dis_train]],"\n")
  }
  cat("=====================================\n")
  cat("The performance of logistic Regression is:","\n")
  print(rowMeans(input_object$performance$performance_log))
  cat("\n")
  cat("The performance of SVM is:","\n")
  print(rowMeans(input_object$performance$performance_svm))
  cat("\n")
  cat("The performance of CRT is:","\n")
  print(rowMeans(input_object$performance$performance_CRT))
  cat("The performance of Neural Network is:","\n")
  print(rowMeans(input_object$performance$performance_NNET)) 
  cat("The performance of TAN is:","\n")
  print(rowMeans(input_object$performance$performance_tan)) 
  cat("=====================================\n")
  cat("Ouput of Analysis:\n",input_object$settings["Out_put_file","content"],"\n")
  cat("=====================================\n")
  cat("Important Features achieved by specific Machine Learning algorithms: \n") 
  if(as.numeric(input_object$parameters["FS_Method_1","content"])>0){
    cat("Random Forest :\n",input_object$settings["Random_Forrest_Location","content"],"\n")}
  if(as.numeric(input_object$parameters["FS_Method_2","content"])>0){
    cat("LASSO :\n",input_object$settings["Lasso_location","content"],"\n")}
  if(as.numeric(input_object$parameters["FS_Method_3","content"])>0){
    cat("Fast Feature Selection :\n",input_object$settings["Fast_Feature_Location","content"],"\n")}
  if(as.numeric(input_object$parameters["FS_Method_4","content"])>0){
    cat("SVM Features :\n",input_object$settings["itself_svm_Location","content"],"\n")
    cat("CRT Features :\n",input_object$settings["itself_CRT_Location","content"],"\n")
    cat("Logistic Features :\n",input_object$settings["itself_log_Location","content"],"\n")
    cat("Neural Network Features :\n",input_object$settings["itself_NNET_Location","content"],"\n")
    cat("TAN Features :\n",input_object$settings["itself_tan_Location","content"],"\n")}
  
  if(as.numeric(input_object$parameters["FS_Method_5","content"])>0){
    cat("Literature :\n",input_object$settings["Literature_file","content"],"\n")}
  if(as.numeric(input_object$parameters["FS_Method_4","content"])>0){
    cat("Consolidation of all the Features :\n",input_object$settings["consolidate_Location","content"],"\n")}
  
  cat("========================================================================== \n")
  cat("Location of the (train/test) folds used in the prediction: \n") 
  for(i in 1:n_folds){
    balanced_data_train<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_multi,"_","_",is_bal,"_","fold-",i,"_","balanced-train.csv"),collapse = "")
    balanced_data_test<-paste(c(analysis_folder,"balanced data/","_",method_fs,"_",is_multi,"_","_",is_bal,"_","fold-",i,"_","balanced-test.csv"),collapse = "")
    cat("Location of balanced train fold: ",i, balanced_data_train,"\n")
    cat("Location of balanced test fold: ",i, balanced_data_test,"\n")
  }
  
  cat("========================================================================== \n")
  cat("Performance of Machine Learning Algorithms in predictions: \n")
  cat("Performance of the Logistic Regression :\n",input_object$settings["log_perf_folds_file","content"],"\n")
  cat("Performance of the SVM :\n",input_object$settings["svm_perf_folds_file","content"],"\n")
  cat("Performance of the CRT :\n",input_object$settings["CRT_perf_folds_file","content"],"\n")
  cat("Performance of the Neural Network :\n",input_object$settings["NNET_perf_folds_file","content"],"\n")
  cat("Performance of the TAN :\n",input_object$settings["tan_perf_folds_file","content"],"\n")
  cat("========================================================================== \n")
  cat("Importance of the selected features used by specific Machine Learning Algorithms for prediction: \n")
  cat("Important variables in Logistic regression :\n",input_object$settings["log_imp_folds_file","content"],"\n")
  cat("Important variables in SVM :\n",input_object$settings["svm_imp_folds_file","content"],"\n")
  cat("Important variables in CRT :\n",input_object$settings["CRT_imp_folds_file","content"],"\n")
  cat("Important variables in Neural Network :\n",input_object$settings["NNET_imp_folds_file","content"],"\n")
  cat("Important variables in TAN :\n",input_object$settings["tan_imp_folds_file","content"],"\n")
  cat("========================================================================== \n")
  cat("Weighted mportance of the selected features used by specific Machine Learning Algorithms for prediction (Weighted on AUC) : \n")
  cat("Weighted Performance and Average of the Logistic Regression :\n",input_object$settings["Avg_log_perf_folds_file","content"],"\n")
  cat("Weighted Performance and Average of the SVM :\n",input_object$settings["Avg_svm_perf_folds_file","content"],"\n")
  cat("Weighted Performance and Average of the CRT :\n",input_object$settings["Avg_CRT_perf_folds_file","content"],"\n")
  cat("Weighted Performance and Average of the Neural Network :\n",input_object$settings["Avg_NNET_perf_folds_file","content"],"\n")
  cat("Weighted Performance and Average of the TAN :\n",input_object$settings["Avg_tan_perf_folds_file","content"],"\n")
  cat("svm_sigma is : ",input_object$svm_sigma, "SVM cost is : ",input_object$svm_cost)
  # colsing the file
  sink()
  
  #sending email to Hamid and Bin about the predicted stock
  
  send.mail(from = "auburn.datascience@gmail.com",
            to = c("hamid.ahady@gmail.com"),
            subject = c( input_object$settings["Out_put_file_name","content"]),
            body = sprintf("The address of the output file is:  %s", input_object$settings["Out_put_file","content"]) ,
            
            smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "auburn.datascience", passwd = "machinelearning", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = input_object$settings["Out_put_file","content"],
            file.names = input_object$settings["Out_put_file_name","content"], # optional parameter
            file.descriptions = c("It contains output of the analysis that you set"), # optional parameter
            debug = TRUE)
  
  #======================================
  
  cat("Ouput of Analysis:\n",input_object$settings["Out_put_file","content"],"\n")
  return(input_object)
  
}

# input_object<-Report(Bino_pred(multi_pred(Lit(consolidate(itself(Lasso_Binomial(Lasso_Multinomial(Random_Forrest(FFS(ADP(data_cleaning(initialization(reading())))))))))))))
# input_object<-Lit(consolidate(itself(Lasso_Binomial(Lasso_Multinomial(Random_Forrest(FFS(data_cleaning(initialization(reading())))))))))
# 
# input_object<-reading()
# input_object<-initialization(input_object)
# input_object<-data_cleaning(input_object)
# input_object<-ADP(input_object)
# input_object<-FFS(input_object) 
# input_object<-Random_Forrest(input_object)
# input_object<-Lasso_Multinomial(input_object)
# input_object<-Lasso_Binomial(input_object)
# input_object<-itself(input_object)
# input_object<-consolidate(input_object)
# input_object<-multi_pred(input_object)
# input_object<-Bino_pred(input_object)
# Report(input_object)
# 
# input_object$Random_Forrest
# input_object$Lasso
# input_object$FFS
# input_object$itself$svm
# input_object$itself$log
# input_object$itself$crt
# input_object$itself$tan
# input_object$itself$nnet
