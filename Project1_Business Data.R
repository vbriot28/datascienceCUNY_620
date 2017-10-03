#Load Business Data"

load("yelp_business.RData")

# run some basic test on business data frame

dim(business)

#We have 15 columns but some are nested structures

# Column 15: Type is always = 'Business' and therefore irrelevant
# Column 14 is a structure that needs to be split/spread to have individual attributes
# Column 5: Categories needs to be dealt with to subselect a particular business
# Column 3: hours will not be keps
# Load library for data manipulation dplyr, tidyr

library(dplyr)
library(tidyr)

df_business <- business %>% select(business_id, full_address, open, categories, city, review_count, name, neighborhoods, longitude, latitude, state, stars)

df_Attributes <- as.data.frame(business[,14])

# Column 12 of df2 is itself another structure, we will not consider it
# Select column 14, 17, Waiter-Service

df_Attributes_subset <- df_Attributes %>% select(Delivery, `Take-out`, `Waiter Service`)

df_business_categories <- cbind(df_business,df_Attributes_subset)

dim(df_business_categories)

df_Restaurants <- filter(df_business_categories, mapply(function(x) "Restaurants" %in% unlist(x), categories))

df_Restaurants %>% group_by(city) %>% summarise(count_city = n()) %>% arrange(desc(count_city))
                                                                              
df_LasVegas <- df_Restaurants %>% filter(city=="Las Vegas")

#Identify which is column is a list
typeof(df_LasVegas$business_id)
typeof(df_LasVegas$full_address)
typeof(df_LasVegas$open)
typeof(df_LasVegas$categories)
typeof(df_LasVegas$city)
typeof(df_LasVegas$review_count)
typeof(df_LasVegas$name)
typeof(df_LasVegas$neighborhoods)
typeof(df_LasVegas$longitude)
typeof(df_LasVegas$latitude)
typeof(df_LasVegas$state)
typeof(df_LasVegas$stars)
typeof(df_LasVegas$Delivery)
typeof(df_LasVegas$`Take-out`)
typeof(df_LasVegas$`Waiter Service`)



df_LasVegas$categories <- vapply(df_LasVegas$categories, paste, collapse = ", ", character(1L))
df_LasVegas$full_address <- vapply(df_LasVegas$full_address, paste, collapse = ", ", character(1L))
df_LasVegas$name <- vapply(df_LasVegas$name, paste, collapse = ", ", character(1L))
df_LasVegas$neighborhoods <- vapply(df_LasVegas$neighborhoods, paste, collapse = ", ", character(1L))


# df_LasVegas$categories <- sapply(df_LasVegas$categories, unlist)
# df_LasVegas$full_address <- sapply(df_LasVegas$full_address, unlist)
# df_LasVegas$name <- sapply(df_LasVegas$name, unlist)
# df_LasVegas$neighborhoods <- sapply(df_LasVegas$neighborhoods, unlist)

write.csv(df_LasVegas, file = "LasVegas.csv")

library(stringr)

df_LasVegas$add1 <- lapply(strsplit(as.character(df_LasVegas$full_address), ","), "[", 1)
df_LasVegas$add2 <- lapply(strsplit(as.character(df_LasVegas$full_address), ","), "[", 2)
df_LasVegas$addl1 <- lapply(strsplit(as.character(df_LasVegas$add1), "\n"), "[", 1)
df_LasVegas$addl2 <- lapply(strsplit(as.character(df_LasVegas$add1), "\n"), "[", 2)
df_LasVegas$addl3 <- lapply(strsplit(as.character(df_LasVegas$add1), "\n"), "[", 3)

df_LasVegas$add3 <- lapply(df_LasVegas$add2, trimws)

df_LasVegas$addl4 <- lapply(strsplit(as.character(df_LasVegas$add3), " "), "[", 1)
df_LasVegas$zip <- lapply(strsplit(as.character(df_LasVegas$add3), " "), "[", 2)

#Neighborhood - only keep first one if populated
df_LasVegas$neighborhoods <- lapply(strsplit(as.character(df_LasVegas$neighborhoods), ","), "[", 1)
df_LasVegas$neighborhoods2 <- lapply(strsplit(as.character(df_LasVegas$neighborhoods), ","), "[", 2)

#Force Logical to Character"
df_LasVegas$Delivery <- as.character(df_LasVegas$Delivery)
df_LasVegas$`Take-out` <- as.character(df_LasVegas$`Take-out`)
df_LasVegas$`Waiter Service` <- as.character(df_LasVegas$`Waiter Service`)


df_LasVegas <- df_LasVegas %>% mutate(addl2 = ifelse(is.na(addl3), " ", addl2))
df_LasVegas <- df_LasVegas %>% mutate(neighborhoods = ifelse(is.na(neighborhoods), "Unknown", neighborhoods))
df_LasVegas <- df_LasVegas %>% mutate(Delivery = ifelse(is.na(Delivery), "Unknown", Delivery))
df_LasVegas <- df_LasVegas %>% mutate(`Take-out` = ifelse(is.na(`Take-out`), "Unknown", `Take-out`))
df_LasVegas <- df_LasVegas %>% mutate(`Waiter Service` = ifelse(is.na(`Waiter Service`), "Unknown", `Waiter Service`))

df_LasVegas_clean <- df_LasVegas %>% select(business_id, name, addl1, addl2, city, state, zip, neighborhoods, open, review_count, stars, Delivery, `Take-out`, `Waiter Service`, longitude, latitude)

#Identify which is column is a list
typeof(df_LasVegas_clean$business_id)
typeof(df_LasVegas_clean$name)
typeof(df_LasVegas_clean$addl1)
typeof(df_LasVegas_clean$addl2)
typeof(df_LasVegas_clean$city)
typeof(df_LasVegas_clean$state)
typeof(df_LasVegas_clean$zip)
typeof(df_LasVegas_clean$neighborhoods)
typeof(df_LasVegas_clean$open)
typeof(df_LasVegas_clean$review_count)
typeof(df_LasVegas_clean$stars)
typeof(df_LasVegas_clean$Delivery)
typeof(df_LasVegas_clean$`Take-out`)
typeof(df_LasVegas_clean$`Waiter Service`)
typeof(df_LasVegas_clean$longitude)
typeof(df_LasVegas_clean$latitude)

df_LasVegas_clean$addl1 <- vapply(df_LasVegas_clean$addl1, paste, collapse = ", ", character(1L))
df_LasVegas_clean$addl2 <- vapply(df_LasVegas_clean$addl2, paste, collapse = ", ", character(1L))
df_LasVegas_clean$zip <- vapply(df_LasVegas_clean$zip, paste, collapse = ", ", character(1L))
df_LasVegas_clean$neighborhoods <- vapply(df_LasVegas_clean$neighborhoods, paste, collapse = ", ", character(1L))

write.csv(df_LasVegas_clean, file = "LasVegas_clean.csv")





