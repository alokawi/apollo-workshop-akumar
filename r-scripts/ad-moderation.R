#read the data and convert to dataframe
readData <- function(x){
  library(rjson)
  path <- x 
  c <- file(path, "r")
  l <- readLines(c, -1L)
  json <- lapply(X=l, fromJSON)
  
  data = as.data.frame(do.call(rbind, json))
  return(data)
}

#clean the data
cleanData <- function(data){

	names <- c("id","slug","market","locale", "category","location","type","state" ,"posted_at",
             "updated_at","published_at" ,"account","account_type","rejection_reasons" ,
             "images" ,"account_phone_numbers", "version" ,"contact_name","contact_phone","contact_email",
             "price_amount","price_currency","price_negotiable" ,
             "price_unit" ,"title","description","item_type" ,"brand", "model","condition",
             "model_year", "registration_year","transmission", "body","fuels","mileage_unit","mileage_value" ,  
             "engine_capacity_unit" ,"engine_capacity_value","gender","features","authenticity","employment" ,
             "apply_via" ,"salary_amount","salary_unit" ,"salary_currency", "salary_is_negotiable" , "address" ,
             "land_type" , "bedrooms" ,"bathrooms", "size_unit" ,"size_value" )  
  
  
  names(data) <- names
  names(data)
  sapply(data,class)

	data$id=unlist(data$id, recursive = TRUE, use.names = TRUE)
  mode(data$id)
  data$id=as.factor(data$id)
  levels(data$id)
  
  data$slug=unlist(data$slug, recursive = TRUE, use.names = TRUE)
  mode(data$slug)
  data$slug=as.factor(data$slug)
  levels(data$slug)
  
  
  data$market=unlist(data$market, recursive = TRUE, use.names = TRUE)
  mode(data$market)
  #data$market=as.factor(data$market)
  levels(data$market)
  
  data$locale=unlist(data$locale, recursive = TRUE, use.names = TRUE)
  mode(data$locale)
  data$locale=as.factor(data$locale)
  levels(data$locale)
  
  data$category=unlist(data$category, recursive = TRUE, use.names = TRUE)
  mode(data$category)
  #data$category=as.factor(data$category)
  levels(data$category)
  
  #dim(data)
  data$location=unlist(data$location, recursive = TRUE, use.names = TRUE)
  mode(data$location)
  #table(data$location)
  #data=data[!(data$location==3000001|data$location==3000002|data$location==3000003),]
  #dim(data)
  table(data$location)
  #if(data$location==3000001|data$location==3000002|data$location==3000003)
  #{data}
  #data$location=as.factor(data$location)
  levels(data$location)
  #table(data$location)
  
  data$type=unlist(data$type, recursive = TRUE, use.names = TRUE)
  mode(data$type)
  data$type=as.factor(data$type)
  levels(data$type)
  
  data$state=unlist(data$state, recursive = TRUE, use.names = TRUE)
  mode(data$state)
  data$state=as.factor(data$state)
  levels(data$state)
  
  data$posted_at=unlist(data$posted_at, recursive = TRUE, use.names = TRUE)
  mode(data$posted_at)
  data$posted_at=as.factor(data$posted_at)
  levels(data$posted_at)
  
  data$updated_at=unlist(data$updated_at, recursive = TRUE, use.names = TRUE)
  mode(data$updated_at)
  data$updated_at=as.factor(data$updated_at)
  levels(data$updated_at)
  
  
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  data$published_at=nullToNA(data$published_at)
  data$published_at=unlist(data$published_at, recursive = TRUE, use.names = TRUE)
  mode(data$published_at)
  data$published_at=as.factor(data$published_at)
  levels(data$published_at)
  
  
  data$account=unlist(data$account, recursive = TRUE, use.names = TRUE)
  mode(data$account)
  data$account=as.factor(data$account)
  levels(data$account)
  
  data$account_type=unlist(data$account_type, recursive = TRUE, use.names = TRUE)
  mode(data$account_type)
  data$account_type=as.factor(data$account_type)
  levels(data$account_type)
  
  
  data$rejection_reasons=vapply(data$rejection_reasons, paste, collapse = ", ", character(1L))
  mode(data$rejection_reasons)
  data$rejection_reasons=as.factor(data$rejection_reasons)
  levels(data$rejection_reasons)
  
  
  data$images=vapply(data$images, paste, collapse = ", ", character(1L))
  #data$images=unlist(data$images, recursive = TRUE, use.names = TRUE)
  mode(data$images)
  data$images=as.factor(data$images)
  levels(data$images)
  
  data$account_phone_numbers=unlist(data$account_phone_numbers, recursive = TRUE, use.names = TRUE)
  mode(data$account_phone_numbers)
  data$account_phone_numbers=as.factor(data$account_phone_numbers)
  levels(data$account_phone_numbers)
  
  
  data$version=unlist(data$version, recursive = TRUE, use.names = TRUE)
  mode(data$version)
  data$version=as.factor(data$version)
  levels(data$version)
  
  data$contact_name=unlist(data$contact_name, recursive = TRUE, use.names = TRUE)
  mode(data$contact_name)
  data$contact_name=as.factor(data$contact_name)
  levels(data$contact_name)
  
  data$contact_phone=vapply(data$contact_phone, paste, collapse = ", ", character(1L))
  #data$contact_phone=unlist(data$contact_phone, recursive = TRUE, use.names = TRUE)
  mode(data$contact_phone)
  #data$contact_phone=as.factor(data$contact_phone)
  levels(data$contact_phone)
  
  data$contact_email=unlist(data$contact_email, recursive = TRUE, use.names = TRUE)
  mode(data$contact_email)
  data$contact_email=as.factor(data$contact_email)
  levels(data$contact_email)
  
  data$price_amount=unlist(data$price_amount, recursive = TRUE, use.names = TRUE)
  mode(data$price_amount)
  #data$price_amount=as.factor(data$price_amount)
  levels(data$price_amount)
  
  data$price_currency=unlist(data$price_currency, recursive = TRUE, use.names = TRUE)
  mode(data$price_currency)
  data$price_currency=as.factor(data$price_currency)
  levels(data$price_currency)
  
  data$price_negotiable=unlist(data$price_negotiable, recursive = TRUE, use.names = TRUE)
  mode(data$price_negotiable)
  data$price_negotiable=as.factor(data$price_negotiable)
  levels(data$price_negotiable)
  
  data$price_unit=unlist(data$price_unit, recursive = TRUE, use.names = TRUE)
  mode(data$price_unit)
  data$price_unit=as.factor(data$price_unit)
  levels(data$price_unit)
  
  data$title=unlist(data$title, recursive = TRUE, use.names = TRUE)
  mode(data$title)
  data$title=as.factor(data$title)
  levels(data$title)
  
  data$description=unlist(data$description, recursive = TRUE, use.names = TRUE)
  mode(data$description)
  data$description=as.factor(data$description)
  levels(data$description)
  
  data$item_type=vapply(data$item_type, paste, collapse = ", ", character(1L))
  #data$item_type=unlist(data$item_type, recursive = TRUE, use.names = TRUE)
  mode(data$item_type)
  data$item_type=as.factor(data$item_type)
  levels(data$item_type)
  #table(data$item_type)
  
  data$brand=unlist(data$brand, recursive = TRUE, use.names = TRUE)
  mode(data$brand)
  data$brand=as.factor(data$brand)
  levels(data$brand)
  
  data$model=unlist(data$model, recursive = TRUE, use.names = TRUE)
  mode(data$model)
  data$model=as.factor(data$model)
  levels(data$model)
  
  data$condition=unlist(data$condition, recursive = TRUE, use.names = TRUE)
  mode(data$condition)
  data$condition=as.factor(data$condition)
  levels(data$condition)
  
  data$model_year=unlist(data$model_year, recursive = TRUE, use.names = TRUE)
  mode(data$model_year)
  data$model_year=as.factor(data$model_year)
  levels(data$model_year)
  
  #data$registration_year=vapply(data$registration_year, paste, collapse = ", ", character(1L))
  data$registration_year=unlist(data$registration_year, recursive = TRUE, use.names = TRUE)
  mode(data$registration_year)
  #data$registration_year=as.factor(data$registration_year)
  levels(data$registration_year)
  
  data$transmission=unlist(data$transmission, recursive = TRUE, use.names = TRUE)
  mode(data$transmission)
  data$transmission=as.factor(data$transmission)
  levels(data$transmission)
  
  data$body=unlist(data$body, recursive = TRUE, use.names = TRUE)
  mode(data$body)
  data$body=as.factor(data$body)
  levels(data$body)
  
  data$fuels=vapply(data$fuels, paste, collapse = ", ", character(1L))
  #data$fuels=unlist(data$fuels, recursive = TRUE, use.names = TRUE)
  mode(data$fuels)
  data$fuels=as.factor(data$fuels)
  levels(data$fuels)
  
  data$mileage_unit=unlist(data$mileage_unit, recursive = TRUE, use.names = TRUE)
  mode(data$mileage_unit)
  data$mileage_unit=as.factor(data$mileage_unit)
  levels(data$mileage_unit)
  
  data$mileage_value=unlist(data$mileage_value, recursive = TRUE, use.names = TRUE)
  mode(data$mileage_value)
  data$mileage_value=as.factor(data$mileage_value)
  levels(data$mileage_value)
  
  data$engine_capacity_unit=unlist(data$engine_capacity_unit, recursive = TRUE, use.names = TRUE)
  mode(data$engine_capacity_unit)
  data$engine_capacity_unit=as.factor(data$engine_capacity_unit)
  levels(data$engine_capacity_unit)
  
  data$engine_capacity_value=unlist(data$engine_capacity_value, recursive = TRUE, use.names = TRUE)
  mode(data$engine_capacity_value)
  data$engine_capacity_value=as.factor(data$engine_capacity_value)
  
  data$gender=unlist(data$gender, recursive = TRUE, use.names = TRUE)
  mode(data$gender)
  data$gender=as.factor(data$gender)
  levels(data$gender)
  
  data$features=vapply(data$features, paste, collapse = ", ", character(1L))
  #data$features=unlist(data$features, recursive = TRUE, use.names = TRUE)
  mode(data$features)
  data$features=as.factor(data$features)
  levels(data$features)
  #table(data$features)
  
  ##cleaning of Features column from "replica"
  data$features=as.character(data$features)
  for (i in 1:nrow(data))
  {
    if(data$features[i]=="replica" & data$authenticity[i]=="NA")
    {
      data$authenticity[i]=data$features[i];
      data$features[i]="NA"}
  }
  data$features=as.factor(data$features)
  
  levels(data$features)
  
  data$authenticity=unlist(data$authenticity, recursive = TRUE, use.names = TRUE)
  mode(data$authenticity)
  data$authenticity=as.factor(data$authenticity)
  levels(data$authenticity)
  
  data$employment=unlist(data$employment, recursive = TRUE, use.names = TRUE)
  mode(data$employment)
  data$employment=as.factor(data$employment)
  levels(data$employment)
  
  data$apply_via=vapply(data$apply_via, paste, collapse = ", ", character(1L))
  #data$apply_via=unlist(data$apply_via, recursive = TRUE, use.names = TRUE)
  mode(data$apply_via)
  data$apply_via=as.factor(data$apply_via)
  levels(data$apply_via)
  
  data$salary_amount=unlist(data$salary_amount, recursive = TRUE, use.names = TRUE)
  mode(data$salary_amount)
  #data$salary_amount=as.factor(data$salary_amount)
  levels(data$salary_amount)
  
  data$salary_unit=unlist(data$salary_unit, recursive = TRUE, use.names = TRUE)
  mode(data$salary_unit)
  data$salary_unit=as.factor(data$salary_unit)
  levels(data$salary_unit)
  
  data$salary_currency=unlist(data$salary_currency, recursive = TRUE, use.names = TRUE)
  mode(data$salary_currency)
  data$salary_currency=as.factor(data$salary_currency)
  levels(data$salary_currency)
  
  data$salary_is_negotiable=unlist(data$salary_is_negotiable, recursive = TRUE, use.names = TRUE)
  mode(data$salary_is_negotiable)
  data$salary_is_negotiable=as.factor(data$salary_is_negotiable)
  levels(data$salary_is_negotiable)
  
  data$address=unlist(data$address, recursive = TRUE, use.names = TRUE)
  mode(data$address)
  data$address=as.factor(data$address)
  levels(data$address)
  
  data$land_type=vapply(data$land_type, paste, collapse = ", ", character(1L))
  #data$land_type=unlist(data$land_type, recursive = TRUE, use.names = TRUE)
  mode(data$land_type)
  data$land_type=as.factor(data$land_type)
  levels(data$land_type)
  
  ##cleaning of Item type column from land type information and assigning accordingly land type information
  levels(data$item_type)
  data$item_type=as.character(data$item_type)
  data$land_type=as.character(data$land_type)
  for (i in 1:nrow(data))
  {
    if(data$item_type[i]=="agricultural" |
       data$item_type[i]=="agricultural, commercial"|
       data$item_type[i]=="agricultural, commercial, other"|
       data$item_type[i]=="agricultural, commercial, residential"|
       data$item_type[i]=="agricultural, commercial, residential, other"|
       data$item_type[i]=="agricultural, other"|
       data$item_type[i]=="agricultural, other, residential"|
       data$item_type[i]=="agricultural, residential"|
       data$item_type[i]=="agricultural, residential, other"|
       data$item_type[i]=="commercial"|
       data$item_type[i]=="commercial, agricultural"|
       data$item_type[i]=="commercial, agricultural, other" |
       data$item_type[i]=="commercial, other"|
       data$item_type[i]=="commercial, residential"|
       data$item_type[i]=="commercial, residential, other"|
       data$item_type[i]=="residential"|
       data$item_type[i]=="residential, agricultural"|
       data$item_type[i]=="residential, agricultural, other" |
       data$item_type[i]=="residential, commercial" |
       data$item_type[i]=="residential, commercial, agricultural"|
       data$item_type[i]=="residential, commercial, agricultural, other"|
       data$item_type[i]=="residential, commercial, other" |
       data$item_type[i]=="residential, other")
    {
      data$land_type[i]=data$item_type[i];
      data$item_type[i]="NA"}
  }
  data$land_type=as.factor(data$land_type)
  data$item_type=as.factor(data$item_type)
  levels(data$land_type)
  levels(data$item_type)
  data$bedrooms=unlist(data$bedrooms, recursive = TRUE, use.names = TRUE)
  mode(data$bedrooms)
  data$bedrooms=as.factor(data$bedrooms)
  levels(data$bedrooms)
  
  data$bathrooms=unlist(data$bathrooms, recursive = TRUE, use.names = TRUE)
  mode(data$bathrooms)
  data$bathrooms=as.factor(data$bathrooms)
  levels(data$bathrooms)
  
  data$size_unit=unlist(data$size_unit, recursive = TRUE, use.names = TRUE)
  mode(data$size_unit)
  data$size_unit=as.factor(data$size_unit)
  levels(data$size_unit)
  
  data$size_value=unlist(data$size_value, recursive = TRUE, use.names = TRUE)
  mode(data$size_value)
  data$size_value=as.factor(data$size_value)
  levels(data$size_value)
  
  sapply(data,class)
  
  data$Ad_Status <- ifelse(data$rejection_reasons == "account_over_limit"|
                             data$rejection_reasons == "blacklisted_account"|
                             data$rejection_reasons == "both_for_sale_and_to_sale_items"|
                             data$rejection_reasons == "duplicate"|
                             data$rejection_reasons == "duplicate, repost"|
                             data$rejection_reasons == "duplicate, unrealistic_offer"|         
                             data$rejection_reasons == "fraud"|                                
                             data$rejection_reasons == "illegal"|                             
                             data$rejection_reasons == "invalid_email"|                       
                             data$rejection_reasons == "invalid_phone_number"|                 
                             data$rejection_reasons == "invalid_phone_number, outside_market"|
                             data$rejection_reasons == "marketing"|                            
                             data$rejection_reasons == "missing_details"|                      
                             data$rejection_reasons == "multiple_items"|                      
                             data$rejection_reasons == "multiple_items, marketing"|            
                             data$rejection_reasons == "multiple_items, missing_details"|      
                             data$rejection_reasons == "other"|                               
                             data$rejection_reasons == "outside_market"|                       
                             data$rejection_reasons == "outside_market, spam"|                 
                             data$rejection_reasons == "repost"|                              
                             #data$rejection_reasons == "spam"|                                 
                             #data$rejection_reasons == "spam, too_vague"|                      
                             data$rejection_reasons == "too_vague"|                           
                             data$rejection_reasons == "unrealistic_offer"|                    
                             data$rejection_reasons == "unrealistic_offer, marketing"|         
                             data$rejection_reasons == "unrealistic_offer, missing_details"|  
                             data$rejection_reasons == "unrealistic_offer, multiple_items"|    
                             data$rejection_reasons == "unrealistic_offer, spam"|              
                             data$rejection_reasons == "unverified_account"|                  
                             data$rejection_reasons == "work_from_home_job"|                   
                             data$rejection_reasons == "wrong_category  , too_vague", "rejected", "accepted")
  
  
  table(data$Ad_Status)
  data$Ad_Status <- as.factor(data$Ad_Status)
  table(data$Ad_Status)
  data$rejected_reason1 <-ifelse(data$rejection_reasons == "account_over_limit",1,
                                 ifelse(data$rejection_reasons == "blacklisted_account",2,
                                        ifelse(data$rejection_reasons == "both_for_sale_and_to_sale_items",3,
                                               ifelse(data$rejection_reasons == "duplicate",4,
                                                      ifelse(data$rejection_reasons == "duplicate, repost",5,
                                                             ifelse(data$rejection_reasons == "duplicate, unrealistic_offer",6,         
                                                                    ifelse(data$rejection_reasons == "fraud",7,                                
                                                                           ifelse(data$rejection_reasons == "illegal",8,                             
                                                                                  ifelse(data$rejection_reasons == "invalid_email",9,                       
                                                                                         ifelse(data$rejection_reasons == "invalid_phone_number",10,                 
                                                                                                ifelse(data$rejection_reasons == "invalid_phone_number, outside_market",11,
                                                                                                       ifelse(data$rejection_reasons == "marketing",12,                            
                                                                                                              ifelse(data$rejection_reasons == "missing_details",13,                      
                                                                                                                     ifelse(data$rejection_reasons == "multiple_items",14,                      
                                                                                                                            ifelse(data$rejection_reasons == "multiple_items, marketing",15,            
                                                                                                                                   ifelse(data$rejection_reasons == "multiple_items, missing_details",16,     
                                                                                                                                          ifelse(data$rejection_reasons == "other",17,                               
                                                                                                                                                 ifelse(data$rejection_reasons == "outside_market",18,                       
                                                                                                                                                        ifelse(data$rejection_reasons == "outside_market, spam",19,                 
                                                                                                                                                               ifelse(data$rejection_reasons == "repost",20,                              
                                                                                                                                                                      #ifelse(data$rejection_reasons == "spam",21,                                 
                                                                                                                                                                      #ifelse(data$rejection_reasons == "spam, too_vague",22,                      
                                                                                                                                                                      ifelse(data$rejection_reasons == "too_vague",23,                           
                                                                                                                                                                             ifelse(data$rejection_reasons == "unrealistic_offer",24,                    
                                                                                                                                                                                    ifelse(data$rejection_reasons == "unrealistic_offer, marketing",25,         
                                                                                                                                                                                           ifelse(data$rejection_reasons == "unrealistic_offer, missing_details",26,  
                                                                                                                                                                                                  ifelse(data$rejection_reasons == "unrealistic_offer, multiple_items",27,    
                                                                                                                                                                                                         ifelse(data$rejection_reasons == "unrealistic_offer, spam",28,              
                                                                                                                                                                                                                ifelse(data$rejection_reasons == "unverified_account",29,                  
                                                                                                                                                                                                                       ifelse(data$rejection_reasons == "work_from_home_job",30,                   
                                                                                                                                                                                                                              ifelse(data$rejection_reasons == "wrong_category  , too_vague",31,32)))))))))))))))))))))))))))))
  
data
  return(data)
	#return(readDataOut)
}

#training data
trainData <- function(cleanData){
  library(rpart)

  sapply(cleanData,class)
  
  mytree11 <- rpart(rejected_reason1 ~ slug+contact_name+account_phone_numbers+contact_phone+contact_email+price_amount+
                      location+type+state+locale+images+description+item_type+brand+model+model_year+
                      body+fuels+mileage_value+engine_capacity_value+features+authenticity+employment+salary_amount+bathrooms+bedrooms+
                      address+size_value+posted_at, data = cleanData, cp = -5,xval = 0)
  mytree11$cptable
  mytree11$variable.importance
  printcp(mytree11)
	mytree11
  return(mytree11)

}

#Testing 
testData <- function(trainResult, testing_data){
	testResult <- "confidenceScore, RejectReason"
	testResult
testing_data$rejected_reason_Prediction = predict(trainResult, newdata = testing_data) #Returns the predicted class
  
  testing_data$rejected_reason_InWords<-ifelse(testing_data$rejected_reason_Prediction >= 0.01 & testing_data$rejected_reason_Prediction <= 1,"account_over_limit",
                                               ifelse(testing_data$rejected_reason_Prediction >= 1.01 & testing_data$rejected_reason_Prediction <= 2,"blacklisted_account",
                                                      ifelse(testing_data$rejected_reason_Prediction >= 2.01 & testing_data$rejected_reason_Prediction <= 3,"both_for_sale_and_to_sale_items",
                                                             ifelse(testing_data$rejected_reason_Prediction >= 3.01 & testing_data$rejected_reason_Prediction <= 4,"duplicate",
                                                                    ifelse(testing_data$rejected_reason_Prediction >= 4.01 & testing_data$rejected_reason_Prediction <= 5,"duplicate, repost",
                                                                           ifelse(testing_data$rejected_reason_Prediction >= 5.01 & testing_data$rejected_reason_Prediction <= 6,"duplicate, unrealistic_offer",         
                                                                                  ifelse(testing_data$rejected_reason_Prediction >= 6.01 & testing_data$rejected_reason_Prediction <= 7,"fraud",                                
                                                                                         ifelse(testing_data$rejected_reason_Prediction >= 7.01 & testing_data$rejected_reason_Prediction <= 8,"illegal",                             
                                                                                                ifelse(testing_data$rejected_reason_Prediction >= 8.01 & testing_data$rejected_reason_Prediction <=9,"invalid_email",                       
                                                                                                       ifelse(testing_data$rejected_reason_Prediction >= 9.01 & testing_data$rejected_reason_Prediction <= 10,"invalid_phone_number",                 
                                                                                                              ifelse(testing_data$rejected_reason_Prediction >= 10.01 & testing_data$rejected_reason_Prediction <= 11,"invalid_phone_number, outside_market",
                                                                                                                     ifelse(testing_data$rejected_reason_Prediction >= 11.01 & testing_data$rejected_reason_Prediction <= 12,"marketing",                            
                                                                                                                            ifelse(testing_data$rejected_reason_Prediction >= 12.01 & testing_data$rejected_reason_Prediction <= 13,"missing_details",                      
                                                                                                                                   ifelse(testing_data$rejected_reason_Prediction >= 13.01 & testing_data$rejected_reason_Prediction <= 14,"multiple_items",                      
                                                                                                                                          ifelse(testing_data$rejected_reason_Prediction >= 14.01 & testing_data$rejected_reason_Prediction <= 15,"multiple_items, marketing",            
                                                                                                                                                 ifelse(testing_data$rejected_reason_Prediction >= 15.01 & testing_data$rejected_reason_Prediction <= 16,"multiple_items, missing_details",     
                                                                                                                                                        ifelse(testing_data$rejected_reason_Prediction >= 16.01 & testing_data$rejected_reason_Prediction <= 17,"other",                               
                                                                                                                                                               ifelse(testing_data$rejected_reason_Prediction >= 17.01 & testing_data$rejected_reason_Prediction <= 18,"outside_market",                       
                                                                                                                                                                      ifelse(testing_data$rejected_reason_Prediction >= 18.01 & testing_data$rejected_reason_Prediction <= 19,"outside_market, spam",                 
                                                                                                                                                                             ifelse(testing_data$rejected_reason_Prediction >= 19.01 & testing_data$rejected_reason_Prediction <= 20,"repost",                              
                                                                                                                                                                                    #ifelse(testing_data$rejected_reason_Prediction >= 20.01 & testing_data$rejected_reason_Prediction <= 21,"spam",                                 
                                                                                                                                                                                    #ifelse(testing_data$rejected_reason_Prediction >= 21.01 & testing_data$rejected_reason_Prediction <= 22,"spam, too_vague",                      
                                                                                                                                                                                    ifelse(testing_data$rejected_reason_Prediction >= 22.01 & testing_data$rejected_reason_Prediction <= 23,"too_vague",                           
                                                                                                                                                                                           ifelse(testing_data$rejected_reason_Prediction >= 23.01 & testing_data$rejected_reason_Prediction <= 24,"unrealistic_offer",                    
                                                                                                                                                                                                  ifelse(testing_data$rejected_reason_Prediction >= 24.01 & testing_data$rejected_reason_Prediction <= 25,"unrealistic_offer, marketing",         
                                                                                                                                                                                                         ifelse(testing_data$rejected_reason_Prediction >= 25.01 & testing_data$rejected_reason_Prediction <= 26,"unrealistic_offer, missing_details",  
                                                                                                                                                                                                                ifelse(testing_data$rejected_reason_Prediction >= 26.01 & testing_data$rejected_reason_Prediction <= 27,"unrealistic_offer, multiple_items",    
                                                                                                                                                                                                                       ifelse(testing_data$rejected_reason_Prediction >= 27.01 & testing_data$rejected_reason_Prediction <= 28,"unrealistic_offer, spam",              
                                                                                                                                                                                                                              ifelse(testing_data$rejected_reason_Prediction >= 28.01 & testing_data$rejected_reason_Prediction <= 29,"unverified_account",                  
                                                                                                                                                                                                                                     ifelse(testing_data$rejected_reason_Prediction >= 29.01 & testing_data$rejected_reason_Prediction <= 30,"work_from_home_job",                   
                                                                                                                                                                                                                                            ifelse(testing_data$rejected_reason_Prediction >= 30.01 & testing_data$rejected_reason_Prediction <= 31,"wrong_category","NONE")))))))))))))))))))))))))))))
  
  
  testing_data$Predicted_Ad_Status <- ifelse(testing_data$rejected_reason_Prediction > 31,"accepted","rejected")
  
  testing_data$Variation_Rejection_Status <- ifelse(testing_data$Ad_Status=="rejected" & testing_data$Predicted_Ad_Status=="accepted"|
                                                      testing_data$Ad_Status=="accepted" & testing_data$Predicted_Ad_Status=="rejected" , "Variation_Rejection", "Correct")
  
	testing_data  
	testResult <- testing_data
	return(testResult)
}
