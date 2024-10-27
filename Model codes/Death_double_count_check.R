library(dplyr)

res_death_SQ_White <- res %>%
  select(time, intervention, race, death)

res_death_SQ_Black <- res1 %>%
  select(time, intervention, race, death)

res_death_Wegovy_White <- res2 %>%
  select(time, intervention, race, death)

res_death_Wegovy_Black <- res3 %>%
  select(time, intervention, race, death)

res_death_BS_White <- res4 %>%
  select(time, intervention, race, death)

res_death_BS_Black <- res5 %>%
  select(time, intervention, race, death)

res_death_SQ_White$SQ_death = res_death_SQ_White$death
res_death_Wegovy_White$Wegovy_death = res_death_Wegovy_White$death
res_death_BS_White$BS_death = res_death_BS_White$death

# Select only the columns needed for the new dataframe (time and renamed death columns)
df_SQ <- res_death_SQ_White[, c('time', 'SQ_death')]
df_Wegovy <- res_death_Wegovy_White[, c('time', 'Wegovy_death')]
df_BS <- res_death_BS_White[, c('time', 'BS_death')]

# Merge the dataframes by time
death_data_White <- Reduce(function(x, y) merge(x, y, by='time'), 
                               list(df_SQ, df_Wegovy, df_BS))

death_data_White$SQW = death_data_White$Wegovy_death / death_data_White$SQ_death
death_data_White$SQBS = death_data_White$BS_death / death_data_White$SQ_death

###Now with Black data

res_death_SQ_Black$SQ_death = res_death_SQ_Black$death
res_death_Wegovy_Black$Wegovy_death = res_death_Wegovy_Black$death
res_death_BS_Black$BS_death = res_death_BS_Black$death

# Select only the columns needed for the new dataframe (time and renamed death columns)
df_SQ <- res_death_SQ_Black[, c('time', 'SQ_death')]
df_Wegovy <- res_death_Wegovy_Black[, c('time', 'Wegovy_death')]
df_BS <- res_death_BS_Black[, c('time', 'BS_death')]

# Merge the dataframes by time
death_data_Black <- Reduce(function(x, y) merge(x, y, by='time'), 
                           list(df_SQ, df_Wegovy, df_BS))

death_data_Black$SQW = death_data_Black$Wegovy_death / death_data_Black$SQ_death
death_data_Black$SQBS = death_data_Black$BS_death / death_data_Black$SQ_death







