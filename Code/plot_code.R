#Data
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")
numData <- imdbData_Clean[sapply(imdbData_Clean, function(x) is.numeric(x))]
str(numData)

#Boxplots
boxplot(scale(numData), las = 2, xaxt = "n")
axis(1, las = 2, at = 1:15, labels = c("no. critic\nfor reviews",  
                              "duration",                 
                              "director\nFB likes",  
                              "actor 3\nFB likes",  
                              "actor 1\nFB likes",  
                              "gross",               
                              "no. voted\nusers",        
                              "cast total\nFB likes",
                              "face no.\nin poster",     
                              "no. user\nfor reviews",
                              "budget",              
                              "title year",          
                              "actor 2\nFB likes",  
                              "imdb score",
                              "movie FB\nlikes"))  

names(numData)

#PC Analysis
# gross
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")
imputed_data <- imdbData_Clean
gross_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,gross_cat)
low <- 0
med <- quantile(imputed_data$gross, 0.33)
high <- quantile(imputed_data$gross, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$gross[i])){
    imputed_data$gross_cat[i] = 'black'
  }
  else if(imputed_data$gross[i] >= low & imputed_data$gross[i] < med){
    imputed_data$gross_cat[i] = 'green'
  }
  else if(imputed_data$gross[i] >= med & imputed_data$gross[i] < high){
    imputed_data$gross_cat[i] = 'blue'
  }
  else if(imputed_data$gross[i] >= high){
    imputed_data$gross_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16, 
     main = "coloured by gross"
)

plot(pmovie$scores[, 1], pmovie$scores[, 3],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC3", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)

plot(pmovie$scores[, 1], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)

plot(pmovie$scores[, 2], pmovie$scores[, 3],
     ylim = range(-10,8),
     xlab = "PC2", ylab = "PC3", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)

plot(pmovie$scores[, 2], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC2", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)

plot(pmovie$scores[, 3], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC3", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$gross_cat, pch = 16
)


# budget
imdbData_Clean <- read.csv("Data/Clean IMDb Data.csv")
imputed_data <- imdbData_Clean
budget_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,budget_cat)
low <- 0
med <- quantile(imputed_data$budget, 0.33)
high <- quantile(imputed_data$budget, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$budget[i])){
    imputed_data$budget_cat[i] = 'black'
  }
  else if(imputed_data$budget[i] >= low & imputed_data$budget[i] < med){
    imputed_data$budget_cat[i] = 'green'
  }
  else if(imputed_data$budget[i] >= med & imputed_data$budget[i] < high){
    imputed_data$budget_cat[i] = 'blue'
  }
  else if(imputed_data$budget[i] >= high){
    imputed_data$budget_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16, 
     main = "coloured by budget"
)

plot(pmovie$scores[, 1], pmovie$scores[, 3],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC3", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16, 
     main = "coloured by budget"
)

plot(pmovie$scores[, 1], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16, 
     main = "coloured by budget"
)

plot(pmovie$scores[, 2], pmovie$scores[, 3],
     ylim = range(-10,8),
     xlab = "PC2", ylab = "PC3", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16
)

plot(pmovie$scores[, 2], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC2", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16
)

plot(pmovie$scores[, 3], pmovie$scores[, 4],
     ylim = range(-10,8),
     xlab = "PC3", ylab = "PC4", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$budget_cat, pch = 16
)


#same again only for critics reviews 
critics_review_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,critics_review_cat)
low <- 0
med <- quantile(imputed_data$num_critic_for_reviews, 0.33)
high <- quantile(imputed_data$num_critic_for_reviews, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_critic_for_reviews[i])){
    imputed_data$critics_review_cat[i] = 'black'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= low & imputed_data$num_critic_for_reviews[i] < med){
    imputed_data$critics_review_cat[i] = 'green'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= med & imputed_data$num_critic_for_reviews[i] < high){
    imputed_data$critics_review_cat[i] = 'blue'
  }
  else if(imputed_data$num_critic_for_reviews[i] >= high){
    imputed_data$critics_review_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.8, lwd = 2, type = 'p',
     col = imputed_data$critics_review_cat, pch = 16, 
     main = "coloured by critics_review"
)

# movie_facebook_likes
movie_fb_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data, movie_fb_cat)
for(i in 1:nrow(imdbData_Clean)){
  if(imputed_data$movie_facebook_likes[i] == 0){
    imputed_data$movie_fb_cat[i] = 'black'
  }
  else if(imputed_data$movie_facebook_likes[i] != 0){
    imputed_data$movie_fb_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.8, lwd = 2, type = 'p',
     col = imputed_data$movie_fb_cat, pch = 16
)


# num_user_for_reviews
num_users_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,num_users_cat)
low <- 0
med <- quantile(imputed_data$num_user_for_reviews, 0.33)
high <- quantile(imputed_data$num_user_for_reviews, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_user_for_reviews[i])){
    imputed_data$num_users_cat[i] = 'black'
  }
  else if(imputed_data$num_user_for_reviews[i] >= low & imputed_data$num_user_for_reviews[i] < med){
    imputed_data$num_users_cat[i] = 'green'
  }
  else if(imputed_data$num_user_for_reviews[i] >= med & imputed_data$num_user_for_reviews[i] < high){
    imputed_data$num_users_cat[i] = 'blue'
  }
  else if(imputed_data$num_user_for_reviews[i] >= high){
    imputed_data$num_users_cat[i] = 'red'
  }
}

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$num_users_cat, pch = 16, 
     main = "coloured by num_user_for_reviews"
)

# num_voted_users
num_voters_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,num_voters_cat)
low <- 0
med <- quantile(imputed_data$num_voted_users, 0.33)
high <- quantile(imputed_data$num_voted_users, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$num_voted_users[i])){
    imputed_data$num_voters_cat[i] = 'black'
  }
  else if(imputed_data$num_voted_users[i] >= low & imputed_data$num_voted_users[i] < med){
    imputed_data$num_voters_cat[i] = 'green'
  }
  else if(imputed_data$num_voted_users[i] >= med & imputed_data$num_voted_users[i] < high){
    imputed_data$num_voters_cat[i] = 'blue'
  }
  else if(imputed_data$num_voted_users[i] >= high){
    imputed_data$num_voters_cat[i] = 'red'
  }
}
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$num_voters_cat, pch = 16, 
     main = "coloured by num_voted_users"
)

# imdb_score
imdb_score_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,imdb_score_cat)
low <- 0
med <- quantile(imputed_data$imdb_score, 0.33)
high <- quantile(imputed_data$imdb_score, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$imdb_score[i])){
    imputed_data$imdb_score_cat[i] = 'black'
  }
  else if(imputed_data$imdb_score[i] >= low & imputed_data$imdb_score[i] < med){
    imputed_data$imdb_score_cat[i] = 'green'
  }
  else if(imputed_data$imdb_score[i] >= med & imputed_data$imdb_score[i] < high){
    imputed_data$imdb_score_cat[i] = 'blue'
  }
  else if(imputed_data$imdb_score[i] >= high){
    imputed_data$imdb_score_cat[i] = 'red'
  }
}
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$imdb_score_cat, pch = 16, 
     main = "coloured by imdb_score"
)

# director_facebook_likes
director_facebook_likes_cat <- rep(NA, nrow(imdbData_Clean))
imputed_data <- cbind(imputed_data,director_facebook_likes_cat)
low <- 0
med <- quantile(imputed_data$director_facebook_likes, 0.33)
high <- quantile(imputed_data$director_facebook_likes, 0.66)
for(i in 1:nrow(imdbData_Clean)){
  if(is.na(imputed_data$director_facebook_likes[i])){
    imputed_data$director_facebook_likes_cat[i] = 'black'
  }
  else if(imputed_data$director_facebook_likes[i] >= low & imputed_data$director_facebook_likes[i] < med){
    imputed_data$director_facebook_likes_cat[i] = 'green'
  }
  else if(imputed_data$director_facebook_likes[i] >= med & imputed_data$director_facebook_likes[i] < high){
    imputed_data$director_facebook_likes_cat[i] = 'blue'
  }
  else if(imputed_data$director_facebook_likes[i] >= high){
    imputed_data$director_facebook_likes_cat[i] = 'red'
  }
}
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$director_facebook_likes_cat, pch = 16, 
     main = "coloured by director_facebook_likes"
)

plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,5),
     xlim = range(-5,6),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = imputed_data$director_facebook_likes_cat, pch = 16, 
     main = "coloured by director_facebook_likes"
)



# fancycolour plot ----------
legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .01, cex.axis=0.5)
  par <- opar
}
colr <- rev(terrain.colors(100))
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = colr[as.numeric(cut(imputed_data$gross,breaks = 100))], pch = 16)
legend.col(col = colr, lev = imputed_data$gross)

colr <- rev(terrain.colors(100))
plot(pmovie$scores[, 1], pmovie$scores[, 2],
     ylim = range(-10,8),
     xlab = "PC1", ylab = "PC2", cex=0.6, lwd = 2, type = 'p',
     col = colr[as.numeric(cut(imputed_data$gross,breaks = 100))], pch = 16)
grid()
legend.col(col = colr, lev = imputed_data$gross)



