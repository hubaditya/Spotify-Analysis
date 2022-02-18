# loading packages
packages = c('tidyverse', 'data.table', 'gridExtra', 'corrplot', 'DT', 
             'kableExtra', 'nnet', 'car', 'Rtsne', 'caret', 'class', 'e1071', 
             'rpart', 'rpart.plot', 'randomForest', 'xgboost', 'doParallel', 
             'kernlab', 'ranger')
installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages == F)) {
  install.packages(packages[!installed_packages])
}
suppressMessages(invisible(lapply(packages, library, character.only = T)))
rm(installed_packages, packages)

# suppressing warnings
options(warn = -1)

URL = paste0("https://raw.githubusercontent.com/", 
             "rfordatascience/tidytuesday/master/data/", 
             "2020/2020-01-21/spotify_songs.csv")

spotify_df = fread(URL)

# checking data types of columns
str(spotify_df)
unique(spotify_df$track_album_release_date)

max(spotify_df$track_album_release_date)

# checking missing values for each columns
colSums(is.na(spotify_df))

# counting total number of missing rows and removing them
missing_rows = spotify_df[rowSums(is.na(spotify_df)) > 0, ]
spotify_df = spotify_df[complete.cases(spotify_df), ]
rm(missing_rows)

# removing songs whose duration is too huge or less
duration_out = boxplot(spotify_df$duration_ms, 
                             plot = F, range = 3)$out
spotify_df = spotify_df[!spotify_df$duration_ms %in% duration_out, ]
nrow(spotify_df[spotify_df$duration_ms < 60000, ])
spotify_df = spotify_df[spotify_df$duration_ms > 60000, ]
rm(duration_out)

# removing playlist name as they are names given by the users 
# which are highly subjective and add least information
unique(spotify_df$playlist_name)
spotify_df = spotify_df[, -c("playlist_name", "playlist_id")]

# checking duplicates based on track name, artist and release date
duplicate_rows = spotify_df[duplicated(
  spotify_df[, c("track_name", "track_artist", "track_album_release_date")])]
duplicate_rows = duplicate_rows[order(track_name), ]
rm(duplicate_rows)

# returns the mode of categorical values
# if number of occurence is same, returns all of them
mode = function(x, type) {
  if(type == 'subgenre') {
    cat_values = as.factor(x)
    levels(cat_values)[which.max(tabulate(cat_values))] 
  } else {
    cat_values = as.factor(x)
    temp = table(x)
    paste(levels(cat_values)[which(temp == max(temp))], collapse = ',')
  }
}

# aggregating different genre and subgenre in a single row separated by comma
# adding columns for the mode of genre and subgenre for each track
spotify_df = spotify_df %>%
  group_by(track_name, track_artist, track_album_release_date) %>%
  mutate(playlist_genre_new = map_chr(
    playlist_genre, ~toString(setdiff(playlist_genre, .x))), 
    playlist_subgenre_new = map_chr(
    playlist_subgenre, ~toString(setdiff(playlist_subgenre, .x))), 
    genre_mode = mode(playlist_genre, "genre"), 
    subgenre_mode = mode(playlist_subgenre, "subgenre")) %>%
  ungroup()

spotify_df = unite(spotify_df, "playlist_genre", 
               c("playlist_genre", "playlist_genre_new"), 
               sep = ",")
spotify_df = unite(spotify_df, "playlist_subgenre", 
               c("playlist_subgenre", "playlist_subgenre_new"), 
               sep = ",")
spotify_df$playlist_genre = gsub(",$", "", spotify_df$playlist_genre)
spotify_df$playlist_subgenre = gsub(",$", "", spotify_df$playlist_subgenre)

spotify_df = spotify_df[!duplicated(spotify_df[c("track_name", "track_artist", 
                                                 "track_album_release_date")]), ]

# separating date to year, month and day
# assuming the day to be the 1st of the month where missing
# assuming the month to be Jan where month is missing
spotify_df = separate(spotify_df, col = track_album_release_date, 
                      into = c("year", "month", "day"), sep = "-")
colSums(is.na(spotify_df))
spotify_df[is.na(spotify_df)] = "01"
spotify_df[c("year","month", "day")] = sapply(spotify_df[c("year","month", "day")], 
                                               as.integer)

# changing multigenre mode to a single genre based on the euclidean distance
# assigned to the closest the audio features are to the median of concerned genres
audio_features = colnames(spotify_df)[12:23]
single_genre_df = filter(spotify_df, !grepl(",", genre_mode))
multi_genre_df = filter(spotify_df, grepl(",", genre_mode))
median_df = single_genre_df %>%
  select(c('genre_mode', all_of(audio_features))) %>%
  group_by(genre_mode) %>%
  summarise_if(is.numeric, median) %>%
  ungroup()
for(i in 1:nrow(multi_genre_df)) {
  temp = multi_genre_df[i, c('genre_mode', audio_features)]
  multi_genres = strsplit(temp$genre_mode, ",")[[1]]
  dist_vector = c()
  for(j in 1:length(multi_genres)) {
    median_values = filter(median_df, genre_mode == multi_genres[j])
    eucli_dist = dist(rbind(temp[, audio_features], 
                            median_values[, audio_features])[, -c(12)])[1]
    dist_vector = append(dist_vector, eucli_dist)
  }
  multi_genre_df$genre_mode[i] = multi_genres[which(dist_vector == min(dist_vector))]
}
spotify_df = rbind(single_genre_df, multi_genre_df)
rm(median_df, median_values, multi_genre_df, single_genre_df, temp)


str(spotify_df)
table(spotify_df$playlist_genre)
table(spotify_df$genre_mode)

genres = unique(spotify_df$genre_mode)

################ EDA ##############################

# checking the distribution of audio features
plot_list = list()
for (i in 1:length(audio_features)) {
  plot_list[[i]] = ggplot(spotify_df, aes_string(x = audio_features[i])) + 
    geom_density(color = "darkblue", fill = "lightblue") 
}
do.call(grid.arrange, plot_list)

# checking for outliers in audio features
plot_list = list()
for (i in 1:length(audio_features)) {
  plot_list[[i]] = ggplot(spotify_df, aes_string(y = audio_features[i])) + 
    geom_boxplot(color = "darkblue", fill = "lightblue", outlier.colour = "red")
}
do.call(grid.arrange, plot_list)

# getting default percentage of outliers based on boxplot
for (i in 1:length(audio_features)) {
  out_percent = length(boxplot(spotify_df[, c(audio_features[i])], 
                               plot = F, range = 1.5)
                       $out) * 100 / nrow(spotify_df)
  print(paste0("The outlier percentage for ", 
               audio_features[i], " is ", round(out_percent, 2), "%"))
}
rm(out_percent)

# iqr_low = quantile(spotify_df$danceability, probs = 0.25)
# iqr_high = quantile(spotify_df$danceability, probs = 0.75)
# iqr_outlier_range = 1.5 * (iqr_high - iqr_low)
# danceability_out_1 = filter(spotify_df, 
#                             danceability < iqr_low - iqr_outlier_range |  
#                             danceability > iqr_high + iqr_outlier_range)

# checking the range in audio features based on genre
plot_list = list()
for (i in 1:length(audio_features)) {
  plot_list[[i]] = ggplot(spotify_df, 
                          aes_string(x = "genre_mode", y = audio_features[i])) + 
    geom_boxplot(color = "darkblue", fill = "lightblue", outlier.colour = "red")
}
do.call(grid.arrange, plot_list)

# correlation between audio features
spotify_df %>%
  select(all_of(audio_features)) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Avenir')

# audio features over the years
# taking mean for each year
year_features_df = spotify_df %>%
  select(c('year', all_of(audio_features))) %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()
plot_list = list()
for (i in 1:length(audio_features)) {
    plot_list[[i]] = ggplot(year_features_df, 
                            aes_string(x = "year", y = audio_features[i])) + 
      geom_line() 
}
do.call(grid.arrange, plot_list)
rm(year_features_df)

# correlation within genre
# getting median values for each genre and finding correlation between them
genre_audio_df = spotify_df %>%
  select(c('genre_mode', all_of(audio_features)), -c(mode, key)) %>%
  group_by(genre_mode) %>%
  summarise_if(is.numeric, median) %>%
  ungroup() 
genre_audio_df = select(genre_audio_df, -genre_mode)
# scaling the values for better correlation mapping
genre_audio_df = scale(genre_audio_df)
genre_audio_df = t(genre_audio_df)
colnames(genre_audio_df) = sort(unique(spotify_df$genre_mode))

genre_audio_df %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     main = 'Correlation among genre',
                     mar = c(2,2,2,2),
                     family = 'Avenir')
rm(genre_audio_df)

# correlation of popularity with audio features
popularity_features_df = spotify_df %>%
  select(c('track_popularity', all_of(audio_features))) %>%
  group_by(track_popularity) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()
plot_list = list()
for (i in 1:length(audio_features)) {
  plot_list[[i]] = ggplot(popularity_features_df, aes_string(x = "track_popularity", 
                                                 y = audio_features[i])) + 
    geom_point(shape = 18, color = 4) +
    geom_smooth(method = lm,  linetype = "dashed", color = "darkred", se = F)
}
suppressMessages(do.call(grid.arrange, plot_list))
rm(popularity_features_df)

# popularity of genres across the years
year_genre_features_df = spotify_df %>%
  select(c('year', 'genre_mode', 'track_popularity')) %>%
  group_by(year, genre_mode) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()
plot_list = list()
for (i in 1:length(genres)) {
  temp = filter(year_genre_features_df, genre_mode == genres[i])
  if (nrow(temp) > 0) {
    plot_list[[i]] = ggplot(temp, 
                            aes_string(x = "year", y = "track_popularity")) + 
      geom_line() + 
      ggtitle(paste0("Popularity trend for ", genres[i]))
  }
}
do.call(grid.arrange, plot_list)
rm(temp, year_genre_features_df)

# analysis if holiday season impacts any particular genre
popularity_month_df = spotify_df %>%
  select('month', 'genre_mode', 'track_popularity') %>%
  group_by(month, genre_mode) %>%
  summarise(popularity = mean(track_popularity)) %>%
  ungroup()
ggplot(popularity_month_df, aes(x = month, y = popularity)) +
  geom_line(aes(color = genre_mode, linetype = genre_mode))
rm(popularity_month_df)

# trend for number of songs released
song_count_df = spotify_df %>%
  select('year') %>%
  group_by(year) %>%
  summarise(songs_released = n()) %>%
  ungroup()
ggplot(song_count_df, 
       aes_string(x = "year", y = "songs_released")) + 
  geom_line()

# number of songs released for each genre in the last 10 years
song_count_df = spotify_df %>%
  select('year', 'genre_mode') %>%
  filter(year > 2009 & year <= 2019) %>%
  group_by(year, genre_mode) %>%
  summarise(songs_released = n()) %>%
  ungroup()
ggplot(song_count_df, aes(x = year, y = songs_released)) +
  geom_line(aes(color = genre_mode, linetype = genre_mode))
rm(song_count_df, plot_list)

##################### Modeling ##############################

# filtering out data before 1970 due to different spikes as observed in the EDA
summary(spotify_df[, audio_features])
spotify_df = filter(spotify_df, (year > 1970))

# taking only audio features for genre classification
spotify_df = spotify_df %>%
  select(c('genre_mode'), all_of(audio_features))
colnames(spotify_df)[1] = "genre"

# scaling audio features
spotify_df = spotify_df %>%
  mutate_if(is.numeric, scale)

# checking for multicollinearity
logistic_model = multinom(genre ~., data = spotify_df)
vif(logistic_model)

# observing inter-distance of genres and intra-distance within genres
# TSNE algorithm shows very poor clustering
temp = spotify_df %>%
  mutate(ID = row_number())
tsne_fit = temp %>%
  select('ID', all_of(audio_features)) %>%
  column_to_rownames("ID") %>%
  Rtsne(check_duplicates = F)
tsne_df = tsne_fit$Y %>% 
  as.data.frame() %>%
  rename(tSNE1 = "V1", tSNE2 = "V2") %>%
  mutate(ID = row_number())
tsne_df = tsne_df %>%
  inner_join(temp, by = "ID")
tsne_df %>%
  ggplot(aes(x = tSNE1, 
             y = tSNE2,
             color = genre)) +
  geom_point() +
  theme(legend.position = "bottom")
rm(temp, tsne_fit, tsne_df)

# train-test split
index = createDataPartition(spotify_df$genre, p = 0.7, list = F)
train_df = spotify_df[index,]
test_df = spotify_df[-index,]

# Logistic Regression
logistic_model = multinom(genre ~., data = train_df)
predicted_genre = predict(logistic_model, newdata = train_df[-1])
confusionMatrix(data = predicted_genre, 
                reference = as.factor(train_df$genre))$overall[1]
predicted_genre = predict(logistic_model, test_df[-1])
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# calculating p-value
z = summary(logistic_model)$coefficients / summary(logistic_model)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
print(p)
rm(z, p)

# Logistic Regression without key, mode and duration_ms
logistic_model = multinom(genre ~., data = train_df[-c(4, 6, 13)])
predicted_genre = predict(logistic_model, test_df[-c(1, 4, 6, 13)])
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# KNN
predicted_genre = knn(train = train_df[, -1],
                 test = test_df[, -1],
                 cl = train_df$genre,
                 k = 5)
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# Naive Bayes
bayes_model = naiveBayes(as.factor(genre) ~ ., data = train_df)
predicted_genre = predict(bayes_model, test_df[-1])
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# SVM
svm_model = svm(as.factor(genre) ~ ., data = train_df, kernel = "radial")
predicted_genre = predict(svm_model, newdata = train_df[-1])
confusionMatrix(data = predicted_genre, 
                reference = as.factor(train_df$genre))$overall[1]
predicted_genre = predict(svm_model, test_df[-1], type = "C-classification")
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# Decision Tree
dt_model = rpart(genre ~ ., data = train_df)
predicted_genre = predict(dt_model, newdata = train_df[-1])
predicted_genre = colnames(predicted_genre)[apply(predicted_genre, 1, which.max)]
confusionMatrix(data = as.factor(predicted_genre), 
                reference = as.factor(train_df$genre))$overall[1]
predicted_genre = predict(dt_model, newdata = test_df[-1], type = "class")
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))
rpart.plot(dt_model, 
           type = 5, 
           extra = 104,
           box.palette = list(purple = "#490B32",
                              red = "#9A031E",
                              orange = '#FB8B24',
                              dark_blue = "#0F4C5C",
                              blue = "#5DA9E9",
                              grey = '#66717E'),
           leaf.round = 0,
           fallen.leaves = FALSE, 
           branch = 0.3, 
           under = TRUE,
           under.col = 'grey40',
           family = 'Avenir',
           main = 'Genre Decision Tree',
           tweak = 1.2)

# Random Forest
rf_model = randomForest(as.factor(genre) ~ ., data = train_df,  
                        ntree = 500, importance = T)
predicted_genre = predict(rf_model, newdata = train_df[-1])
confusionMatrix(data = predicted_genre, 
                reference = as.factor(train_df$genre))$overall[1]
predicted_genre = predict(rf_model, newdata = test_df[-1])
confusionMatrix(data = predicted_genre, reference = as.factor(test_df$genre))

# XGBoost
xgb_model = xgboost(data = as.matrix(train_df[-1]), 
                    label = as.integer(as.factor(train_df$genre)),
                    nrounds = 25,
                    verbose = FALSE, 
                    params = list(objective = "multi:softmax",
                                  num_class = 6 + 1))
predicted_genre = predict(xgb_model, newdata = as.matrix(train_df[-1]))
predicted_genre = levels(as.factor(train_df$genre))[predicted_genre]
confusionMatrix(data = as.factor(predicted_genre), 
                reference = as.factor(train_df$genre))$overall[1]
predicted_genre = predict(xgb_model, newdata = as.matrix(test_df[-1]))
predicted_genre = levels(as.factor(test_df$genre))[predicted_genre]
confusionMatrix(data = as.factor(predicted_genre), 
                reference = as.factor(test_df$genre))

# comparing feature importance between Decision Tree, Random Forest and XGBoost
importance_dt = data.frame(importance = dt_model$variable.importance)
importance_dt$feature = row.names(importance_dt)
importance_rf = data.frame(importance = randomForest::importance(rf_model, type = 2))
importance_rf$feature = row.names(importance_rf)
importance_xgb = xgb.importance(model = xgb_model)
compare_importance = importance_xgb %>%
  select(Feature, Gain) %>%
  left_join(importance_dt, by = c('Feature' = 'feature')) %>%
  left_join(importance_rf, by = c('Feature' = 'feature')) %>%
  rename('xgboost' = 'Gain',
         'decision_tree' = 'importance',
         'random_forest' = 'MeanDecreaseGini')
compare_importance = compare_importance %>%
  mutate_if(is.numeric, scale) %>%
  pivot_longer(cols = c('xgboost', 'decision_tree', 'random_forest')) %>%
  rename('model' = 'name')
ggplot(compare_importance, aes(x = reorder(Feature, value, na.rm = T), y = value, color = model)) + 
  geom_point(size = 2) + 
  coord_flip() +
  labs(title = 'Variable Importance by Model',
       y = 'Scaled value', x = '')
rm(compare_importance, importance_dt, importance_rf, importance_xgb)

# Hyperparameter tuning - Takes 45 minutes
# creating parallel processing
cl = makePSOCKcluster(6)
registerDoParallel(cl)

fitControl = trainControl(search = 'random', method = "repeatedcv", 
                          number = 5, repeats = 3, allowParallel = T)

xgb_fit = train(genre ~ ., data = train_df, 
                method = "xgbTree", 
                trControl = fitControl, 
                verbose = F, 
                tuneLength = 10)
# nrounds = 910, max_depth = 5, eta = 0.012, gamma = 3.8, 
# colsample_bytree = 0.5, min_child_weight = 14, subsample = 0.85
print(xgb_fit)

svm_fit = train(genre ~ ., data = train_df, 
                method = "svmRadial", 
                trControl = fitControl, 
                verbose = F, 
                tuneLength = 2)
# sigma = 0.01, C = 18.8
print(svm_fit)

rf_fit = train(genre ~ ., data = train_df, 
               method = "ranger", 
               trControl = fitControl, 
               verbose = F, 
               tuneLength = 5)
# min.node.size = 18, mtry = 6
print(rf_fit)

stopCluster(cl)
rm(fitControl,  cl, rf_fit, svm_fit, xgb_fit)

# Stacking
svm_model = svm(as.factor(genre) ~ ., 
                data = train_df, 
                kernel = 'radial', 
                sigma = 0.01, 
                C = 18.8)
svm_pred = predict(svm_model, newdata = train_df[-1])
svm_pred_test = predict(svm_model, newdata = test_df[-1])

rf_model = randomForest(as.factor(genre) ~ ., 
                        data = train_df,  
                        ntree = 500, 
                        importance = T, 
                        mtry = 6, 
                        min.node.size = 18)
rf_pred = predict(rf_model, newdata = train_df[-1])
rf_pred_test = predict(rf_model, newdata = test_df[-1])

xgb_model = xgboost(data = as.matrix(train_df[-1]), 
                    label = as.integer(as.factor(train_df$genre)),
                    nrounds = 910,
                    max_depth = 5,
                    eta = 0.012, 
                    gamma = 3.8,
                    colsample_bytree = 0.5, 
                    min_child_weight = 14, 
                    subsample = 0.85,
                    verbose = FALSE, 
                    params = list(objective = "multi:softmax",
                                  num_class = 6 + 1))
xgb_pred = predict(xgb_model, newdata = as.matrix(train_df[-1]))
xgb_pred = levels(as.factor(train_df$genre))[xgb_pred]
xgb_pred_test = predict(xgb_model, newdata = as.matrix(test_df[-1]))
xgb_pred_test = levels(as.factor(test_df$genre))[xgb_pred_test]

stacked_df = data.frame(svm = svm_pred, 
                        rf = rf_pred,
                        xgb = xgb_pred,
                        genre = train_df[1])
stacked_df_test = data.frame(svm = svm_pred_test, 
                        rf = rf_pred_test,
                        xgb = xgb_pred_test,
                        genre = test_df[1])

logistic_model = multinom(genre ~., data = stacked_df)
predicted_genre = predict(logistic_model, newdata = stacked_df[-4])
confusionMatrix(data = as.factor(predicted_genre), 
                reference = as.factor(stacked_df$genre))$overall[1]
predicted_genre = predict(logistic_model, stacked_df_test[-4])
confusionMatrix(data = predicted_genre, 
                reference = as.factor(stacked_df_test$genre))