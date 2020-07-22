#header <- scan("190624ADeepCut_resnet50_190708Jul8shuffle1_500000.csv", nlines = 1, what = character())
data <- read.csv("Data/Sound_Data_READY/191009_190708_ALT.test.file.csv", skip = 3, header = FALSE)

#Vectorizing the data
frame_col <- data[,1]
thorax_x <- data[,2]
thorax_y <- data[,3]
ab_x <- data[,2]
ab_y <- data[,3]

thorax_x_mean <- mean(thorax_x)
thorax_y_mean <- mean(thorax_y)

frame_len <- length(ab_x) 

wax_x <- data[,5]
wax_y <- data[,6]

ss_x <- data[, 8]
ss_y <- data[,9]

left_knee_x <- data[,11]
left_knee_y <- data[,12]

left_foot_x <- data[,14]
left_foot_y <- data[,15]

right_knee_x <- data[, 17]
right_knee_y <- data[, 18]

right_foot_x <- data[, 20]
right_foot_y <- data[, 21]

picks_theorem <- function(vector1, vector2){
  sum1 <- 0
  for(i in 1:(length(vector1)-1)){
    sum1 <- sum1 + (vector1[i]*vector2[i+1])
  }
  sum2 <- 0
  for(i in 1:(length(vector2) - 1)){
    sum2 <- sum2 + (vector2[i]*vector1[i+1])
  }
  product <- abs(0.5*(sum1 - sum2))
  return(product)
}

# test_x <- c(20, 18, 12, 16, 20)
# test_y <- c(20, 18, 24, 21, 20)
# 
# print(picks_theorem(test_x, test_y))

upperleft <- function(thorax_x, thorax_y, thorax_x_mean, thorax_y_mean, lk_x, lk_y, lk_x_mean, lk_y_mean){
  slope_avg <- (lk_y_mean - thorax_y_mean)/(lk_x_mean - thorax_x_mean)
  slope_0 <- (lk_y - thorax_y)/(lk_x - thorax_x)
  
  intercept_avg <- lk_y_mean - (slope_avg * lk_x_mean)
  intercept_0 <- lk_y - (slope_0 * lk_x)
  
  x_intersect <- (intercept_0 - intercept_avg)/(slope_avg - slope_0)
  y_intersect <- (slope_avg * x_intersect) + intercept_avg
  
  
  #This decides which cricket is higher up
  bigger_k_x <- lk_x
  bigger_k_y <- lk_y
  smaller_k_x <- lk_x_mean
  smaller_k_y <- lk_y_mean
  
  bigger_t_x <- thorax_x
  bigger_t_y <- thorax_y
  smaller_t_x <- thorax_x_mean
  smaller_t_y <- thorax_y_mean
  
  if(lk_y < lk_y_mean){
    bigger_k_y <- lk_y_mean
    bigger_k_x <- lk_x_mean
    smaller_k_x <- lk_x
    smaller_k_y <- lk_y
    
    bigger_t_x <- thorax_x_mean
    bigger_t_y <- thorax_y_mean
    smaller_t_x <- thorax_x
    smaller_t_y <- thorax_y
  }
  
  if(    (smaller_k_x < x_intersect) & (x_intersect < smaller_t_x)){
    if( (smaller_t_y < y_intersect) & (y_intersect < smaller_k_y)){
      triangle1_vector1 <- c(smaller_t_x, bigger_t_x, x_intersect, smaller_t_x)
      triangle1_vector2 <- c(smaller_t_y, bigger_t_y, y_intersect, smaller_t_y)
      triangle2_vector1 <- c(x_intersect, smaller_k_x, bigger_k_x, x_intersect)
      triangle2_vector2 <- c(y_intersect, smaller_k_y, bigger_k_y, y_intersect)
      
      area <- picks_theorem(triangle1_vector1, triangle1_vector2) + picks_theorem(triangle2_vector1, triangle2_vector2)
      return(area)
    }else{
      vector1 <- c(smaller_t_x, bigger_t_x, bigger_k_x, smaller_k_x, smaller_t_x)
      vector2 <- c(smaller_t_y, bigger_t_y, bigger_k_y, smaller_k_y, smaller_t_y)
      area <- picks_theorem(vector1, vector2)
      return(area)}
  }
  else{
    vector1 <- c(smaller_t_x, bigger_t_x, bigger_k_x, smaller_k_x, smaller_t_x)
    vector2 <- c(smaller_t_y, bigger_t_y, bigger_k_y, smaller_k_y, smaller_t_y)
    area <- picks_theorem(vector1, vector2)
    return(area)
  }
  
}

upperright <- function(thorax_x, thorax_y, thorax_x_mean, thorax_y_mean, lk_x, lk_y, lk_x_mean, lk_y_mean){
  slope_avg <- (lk_y_mean - thorax_y_mean)/(lk_x_mean - thorax_x_mean)
  slope_0 <- (lk_y - thorax_y)/(lk_x - thorax_x)
  
  intercept_avg <- lk_y_mean - (slope_avg * lk_x_mean)
  intercept_0 <- lk_y - (slope_0 * lk_x)
  
  x_intersect <- (intercept_0 - intercept_avg)/(slope_avg - slope_0)
  y_intersect <- (slope_avg * x_intersect) + intercept_avg
  
  
  #This decides which cricket is higher up
  bigger_k_x <- lk_x
  bigger_k_y <- lk_y
  smaller_k_x <- lk_x_mean
  smaller_k_y <- lk_y_mean
  
  bigger_t_x <- thorax_x
  bigger_t_y <- thorax_y
  smaller_t_x <- thorax_x_mean
  smaller_t_y <- thorax_y_mean
  
  if(lk_y < lk_y_mean){
    bigger_k_y <- lk_y_mean
    bigger_k_x <- lk_x_mean
    smaller_k_x <- lk_x
    smaller_k_y <- lk_y
    
    bigger_t_x <- thorax_x_mean
    bigger_t_y <- thorax_y_mean
    smaller_t_x <- thorax_x
    smaller_t_y <- thorax_y
  }
  
  if(    (smaller_k_x > x_intersect) & (x_intersect > smaller_t_x)){
    if( (smaller_t_y < y_intersect) & (y_intersect < smaller_k_y)){
      triangle1_vector1 <- c(smaller_t_x, bigger_t_x, x_intersect, smaller_t_x)
      triangle1_vector2 <- c(smaller_t_y, bigger_t_y, y_intersect, smaller_t_y)
      triangle2_vector1 <- c(x_intersect, smaller_k_x, bigger_k_x, x_intersect)
      triangle2_vector2 <- c(y_intersect, smaller_k_y, bigger_k_y, y_intersect)
      
      area <- picks_theorem(triangle1_vector1, triangle1_vector2) + picks_theorem(triangle2_vector1, triangle2_vector2)
      return(area)
    }  else{
      vector1 <- c(smaller_t_x, bigger_t_x, bigger_k_x, smaller_k_x, smaller_t_x)
      vector2 <- c(smaller_t_y, bigger_t_y, bigger_k_y, smaller_k_y, smaller_t_y)
      area <- picks_theorem(vector1, vector2)
      return(area)}
  }
  else{
    vector1 <- c(smaller_t_x, bigger_t_x, bigger_k_x, smaller_k_x, smaller_t_x)
    vector2 <- c(smaller_t_y, bigger_t_y, bigger_k_y, smaller_k_y, smaller_t_y)
    area <- picks_theorem(vector1, vector2)
    return(area)
  }
  
}


body <- function(thorax_x, thorax_y, thorax_x_mean, thorax_y_mean, wax_x, wax_y){
  vector1 <- c(thorax_x, thorax_x_mean, wax_x, thorax_x)
  vector2 <- c(thorax_y, thorax_y_mean, wax_y, thorax_y)
  area <- picks_theorem(vector1, vector2)
  return(area)
}

lowerleft <- function(lk_x, lk_y, lk_x_mean, lk_y_mean, lf_x, lf_y, lf_x_mean, lf_y_mean){
  slope_avg <- (lf_y_mean - lk_y_mean)/(lf_x_mean - lk_x_mean)
  slope_0 <- (lf_y - lk_y)/(lf_x - lk_x)
  
  intercept_avg <- lf_y_mean - (slope_avg * lf_x_mean)
  intercept_0 <- lf_y - (slope_0 * lf_x)
  
  x_intersect <- (intercept_0 - intercept_avg)/(slope_avg - slope_0)
  y_intersect <- (slope_avg * x_intersect) + intercept_avg
  
  
  #This decides which cricket is higher up
  bigger_f_x <- lf_x
  bigger_f_y <- lf_y
  smaller_f_x <- lf_x_mean
  smaller_f_y <- lf_y_mean
  
  bigger_k_x <- lk_x
  bigger_k_y <- lk_y
  smaller_k_x <- lk_x_mean
  smaller_k_y <- lk_y_mean
  
  if(lk_x < lk_x_mean){
    bigger_k_y <- lk_y_mean
    bigger_k_x <- lk_x_mean
    smaller_k_x <- lk_x
    smaller_k_y <- lk_y
    
    bigger_t_x <- thorax_x_mean
    bigger_t_y <- thorax_y_mean
    smaller_t_x <- thorax_x
    smaller_t_y <- thorax_y
  }
  
  if(    (smaller_k_x < x_intersect) & (x_intersect < smaller_f_x)){
    if( (smaller_f_y < y_intersect) & (y_intersect < smaller_k_y)){
      triangle1_vector1 <- c(smaller_k_x, bigger_k_x, x_intersect, smaller_k_x)
      triangle1_vector2 <- c(smaller_k_y, bigger_k_y, y_intersect, smaller_k_y)
      triangle2_vector1 <- c(x_intersect, smaller_f_x, bigger_f_x, x_intersect)
      triangle2_vector2 <- c(y_intersect, smaller_f_y, bigger_f_y, y_intersect)
      
      area <- picks_theorem(triangle1_vector1, triangle1_vector2) + picks_theorem(triangle2_vector1, triangle2_vector2)
      return(area)
    }  else{
      vector1 <- c(smaller_k_x, bigger_k_x, bigger_f_x, smaller_f_x, smaller_k_x)
      vector2 <- c(smaller_k_y, bigger_k_y, bigger_f_y, smaller_f_y, smaller_k_y)
      area <- picks_theorem(vector1, vector2)
      return(area)}
  }
  else{
    vector1 <- c(smaller_k_x, bigger_k_x, bigger_f_x, smaller_f_x, smaller_k_x)
    vector2 <- c(smaller_k_y, bigger_k_y, bigger_f_y, smaller_f_y, smaller_k_y)
    area <- picks_theorem(vector1, vector2)
    return(area)
  }
  
}

lowerright <- function(lk_x, lk_y, lk_x_mean, lk_y_mean, lf_x, lf_y, lf_x_mean, lf_y_mean){
  slope_avg <- (lf_y_mean - lk_y_mean)/(lf_x_mean - lk_x_mean)
  slope_0 <- (lf_y - lk_y)/(lf_x - lk_x)
  
  intercept_avg <- lf_y_mean - (slope_avg * lf_x_mean)
  intercept_0 <- lf_y - (slope_0 * lf_x)
  
  x_intersect <- (intercept_0 - intercept_avg)/(slope_avg - slope_0)
  y_intersect <- (slope_avg * x_intersect) + intercept_avg
  
  
  #This decides which cricket is higher up
  bigger_f_x <- lf_x
  bigger_f_y <- lf_y
  smaller_f_x <- lf_x_mean
  smaller_f_y <- lf_y_mean
  
  bigger_k_x <- lk_x
  bigger_k_y <- lk_y
  smaller_k_x <- lk_x_mean
  smaller_k_y <- lk_y_mean
  
  if(lk_x < lk_x_mean){
    bigger_k_y <- lk_y_mean
    bigger_k_x <- lk_x_mean
    smaller_k_x <- lk_x
    smaller_k_y <- lk_y
    
    bigger_t_x <- thorax_x_mean
    bigger_t_y <- thorax_y_mean
    smaller_t_x <- thorax_x
    smaller_t_y <- thorax_y
  }
  
  if(    (smaller_k_x < x_intersect) & (x_intersect < smaller_f_x)){
    if( (smaller_f_y < y_intersect) & (y_intersect < smaller_k_y)){
      triangle1_vector1 <- c(smaller_k_x, bigger_k_x, x_intersect, smaller_k_x)
      triangle1_vector2 <- c(smaller_k_y, bigger_k_y, y_intersect, smaller_k_y)
      triangle2_vector1 <- c(x_intersect, smaller_f_x, bigger_f_x, x_intersect)
      triangle2_vector2 <- c(y_intersect, smaller_f_y, bigger_f_y, y_intersect)
      
      area <- picks_theorem(triangle1_vector1, triangle1_vector2) + picks_theorem(triangle2_vector1, triangle2_vector2)
      return(area)
    }  else{
      vector1 <- c(smaller_k_x, bigger_k_x, bigger_f_x, smaller_f_x, smaller_k_x)
      vector2 <- c(smaller_k_y, bigger_k_y, bigger_f_y, smaller_f_y, smaller_k_y)
      area <- picks_theorem(vector1, vector2)
      return(area)}
  }
  else{
    vector1 <- c(smaller_k_x, bigger_k_x, bigger_f_x, smaller_f_x, smaller_k_x)
    vector2 <- c(smaller_k_y, bigger_k_y, bigger_f_y, smaller_f_y, smaller_k_y)
    area <- picks_theorem(vector1, vector2)
    return(area)
  }
  
}



dist.func <- function(x1,y1,x2,y2)
{
  sqrt((x2-x1)^2 + (y2-y1)^2) 
}

#find the angle between the ab and the center line 
#angles <- asin(ab_x/(sqrt((wax_y)^2+ (ab_x)^2)))
a <- dist.func(ab_x,ab_y,wax_x,ab_y)
b <- dist.func(wax_x,wax_y,wax_x,ab_y)
c <- dist.func(ab_x,ab_y,wax_x,wax_y)

angles <- acos((b^2+c^2-a^2)/(2*b*c))

for (i in 1:frame_len)
{
  if (ab_x[i] <= wax_x[i])
  {
    angles[i] <- -angles[i]
  }
  
}

angles <- angles*180/pi

T=frame_len
num.frames <- 12
angle.frame <- rep(0, frame_len/num.frames)
for(i in seq(0, T, num.frames))
{
   angle.frame[i/num.frames] <- mean(angles[i:i+num.frames-1])  
}

frames <- seq(1:frame_len)
shots <- rep(0, frame_len/num.frames)

for (i in seq(0, T, num.frames))
{
  shots[i/num.frames] <- i
}


body.twitch <- rep(0, frame_len)
upperleft.twitch <- rep(0, frame_len)
upperright.twitch <- rep(0, frame_len)
lowerleft.twitch <- rep(0, frame_len)
lowerright.twitch <- rep(0, frame_len)
grand.twitch <- rep(0, frame_len)

average.twitch <- rep(0,0)
bodyshot.twitch <- rep(0,0)
upperleftshot.twitch <- rep(0,0)
upperrightshot.twitch <- rep(0,0)
lowerleftshot.twitch <- rep(0,0)
lowerrightshot.twitch <- rep(0,0)


#
for(i in seq(1, T-12,num.frames)){
  ab_x_mean <- mean(ab_x[(i):(i+11)])
  ab_y_mean <- mean(ab_y[(i):(i+11)])
  wax_x_mean <- mean(wax_x[(i):(i+11)])
  wax_y_mean <- mean(wax_y[i:(i+11)])
  left_knee_x_mean <- mean(left_knee_x[i:(i+11)])
  left_knee_y_mean <- mean(left_knee_y[i:(i+11)])
  left_foot_x_mean <- mean(left_foot_x[i:(i+11)])
  left_foot_y_mean <- mean(left_foot_y[i:(i+11)])
  right_knee_x_mean <- mean(right_knee_x[i:(i+11)])
  right_knee_y_mean <- mean(right_knee_y[i:(i+11)])
  right_foot_x_mean <- mean(right_foot_x[i:(i+11)])
  right_foot_y_mean <- mean(right_foot_y[i:(i+11)])
  upperleft.sum <- 0
  upperright.sum <- 0
  lowerleft.sum <- 0
  lowerright.sum <- 0
  body.sum <- 0
  
  for(j in i:(i+11)){
    upperleft.twitch[j] <- upperleft(ab_x[j], ab_y[j], ab_x_mean, ab_y_mean, left_knee_x[j], left_knee_y[j], left_knee_x_mean, left_knee_y_mean)
    upperleft.sum <- upperleft.sum + upperleft.twitch[j]
    
    upperright.twitch[j]<- upperright(ab_x[j], ab_y[j], ab_x_mean, ab_y_mean, right_knee_x[j], right_knee_y[j],
                                      right_knee_x_mean, right_knee_y_mean) 
    upperright.sum <- upperright.sum + upperright.twitch[j]
    
    lowerleft.twitch[j] <- lowerleft(left_knee_x[j], left_knee_y[j], left_knee_x_mean, left_knee_y_mean,
                                     left_foot_x[j], left_foot_y[j], left_foot_x_mean, left_foot_y_mean)
    lowerleft.sum <- lowerleft.sum + lowerleft.twitch[j]
    
    lowerright.twitch[j] <-lowerright(right_knee_x[j], right_knee_y[j], right_knee_x_mean, right_knee_y_mean,
                                      right_foot_x[j], right_foot_y[j], right_foot_x_mean, right_foot_y_mean)
    lowerright.sum <- lowerright.sum + lowerright.twitch[j]
    
    body.twitch[j] <- body(ab_x[j], ab_y[j], ab_x_mean, ab_y_mean, wax_x[j], wax_y[j])
    body.sum <- body.sum + body.twitch[j]
    
    grand.twitch[j] <- upperleft.twitch[j] + upperright.twitch[j] + lowerleft.twitch[j] + lowerright.twitch[j] + body.twitch[j]
  }
  #upperleft.sum <- upperleft.sum/12
  #upperright.sum <- upperright.sum/12
  #lowerleft.sum <- lowerleft.sum/12
  #lowerright.sum <- lowerright.sum/12
  #body.sum <- body.sum/12
  
  upperleftshot.twitch <- c(upperleftshot.twitch, upperleft.sum)
  upperrightshot.twitch <- c(upperrightshot.twitch, upperright.sum)
  lowerleftshot.twitch <- c(lowerleftshot.twitch, lowerleft.sum)
  lowerrightshot.twitch <- c(lowerrightshot.twitch, lowerright.sum)
  bodyshot.twitch <- c(bodyshot.twitch, body.sum)
  
  avg.sum <- upperleft.sum + upperright.sum + lowerleft.sum + lowerright.sum + body.sum
  average.twitch <- c(average.twitch, avg.sum)
}
twitch.frame <- cbind(body.twitch, upperleft.twitch, lowerleft.twitch, upperright.twitch, lowerright.twitch)

#pdf('Figures/FRAMEcricketstatsexample.pdf', width=30,height=20)
# par(mfrow = c(7,1), mar = c(.5,5,.5,.5) ) 
# plot(1:frame_len, angles, type = "l", xlab = "",ylab = "Angle Measurement", col = "mediumorchid4")
# plot(1:frame_len,body.twitch, type = "l", xlab = "", ylab = "Body Twitch", col = "blue")
# plot(1:frame_len,upperleft.twitch, type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green")
# plot(1:frame_len,lowerleft.twitch, type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2")
# plot(1:frame_len,upperright.twitch, type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2")
# plot(1:frame_len,lowerright.twitch, type = "l", xlab = "", ylab = "Lower Right Twitch", col = "red3")
# plot(1:frame_len, ss_x, type = "l", xlab = "Frame Number", ylab = "Sound", col = "midnightblue")
#dev.off()

# pdf('Figures/SHOTcricketexample1.pdf', width=30,height=20)
# par(mfrow = c(3,1), mar = c(.5,5,.5,.5) )
# plot(shots, angle.frame, type = "l", xlab = "",ylab = "Angle Measurement", col = "mediumorchid4")
# plot(shots,bodyshot.twitch, type = "l", xlab = "", ylab = "Body Twitch", col = "blue")
# points(shots,upperleftshot.twitch, type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green")
# points(shots,lowerleftshot.twitch, type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2")
# points(shots,upperrightshot.twitch, type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2")
# points(shots,lowerrightshot.twitch, type = "l", xlab = "", ylab = "Lower Right Twitch", col = "red3")
# points(shots, average.twitch, type = "l", xlab = "", ylab = "Average Twitch")
# plot(1:frame_len, ss_x, type = "l", xlab = "Frame Number", ylab = "Sound", col = "midnightblue")
# dev.off()

#pdf('Figures/SHOTcricketexample1.pdf', width=30,height=20)
# par(mfrow = c(4,1), mar = c(.5,5,.5,.5) )
# plot(shots, angle.frame, type = "l", xlab = "",ylab = "Angle Measurement", col = "mediumorchid4")
# plot(shots,bodyshot.twitch, type = "l", xlab = "", ylab = "Twitch", col = "blue")
# points(shots,upperleftshot.twitch, type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green")
# points(shots,lowerleftshot.twitch, type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2")
# points(shots,upperrightshot.twitch, type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2")
# points(shots,lowerrightshot.twitch, type = "l", xlab = "", ylab = "Lower Right Twitch", col = "red3")
# plot(shots, average.twitch, type = "l", xlab = "", ylab = "Average Twitch")
# plot(1:frame_len, ss_x, type = "l", xlab = "Frame Number", ylab = "Sound", col = "midnightblue")
# #dev.off()

# library(ggplot2)
# twitchtype <- c(rep("bodytwitch" , 477) , rep("upperlefttwich" , 477) , rep("lowerlefttwitch" , 477) , rep("upperrighttwich" , 477), rep("lowerrighttwich",477) )
# condition <- rep(shots, 5)
# value <- c(bodyshot.twitch,upperleftshot.twitch,lowerleftshot.twitch,upperrightshot.twitch,lowerrightshot.twitch)
# test <- data.frame(twitchtype,condition,value)
# 
# pdf('Figures/bargraphtest.pdf', width=30,height=20)
# ggplot(test, aes(fill=condition, y=value, x=twitchtype)) + 
#   geom_bar(position="fill", stat="identity")
# dev.off()

# mat <- matrix(nrow = 5, ncol=shots)
# mat[1,] <- bodyshot.twitch
# mat[2,] <- upperleftshot.twitch
# mat[3,] <- lowerleftshot.twitch
# mat[4,] <- upperrightshot.twitch
# mat[5,] <- lowerrightshot.twitch

bodyshot.twitch.ad <- rep(0,length(shots))
upperleftshot.twitch.ad <-rep(0, length(shots)) 
lowerleftshot.twitch.ad <-rep(0, length(shots)) 
upperrightshot.twitch.ad <-rep(0, length(shots)) 
lowerrightshot.twitch.ad <-rep(0,length(shots)) 

for (i in 1:length(shots))
{
  frac <- bodyshot.twitch[i] + upperleftshot.twitch[i] + lowerleftshot.twitch[i] + upperrightshot.twitch[i] + lowerrightshot.twitch[i]
  bodyshot.twitch.ad[i] <- bodyshot.twitch[i]/frac
  upperleftshot.twitch.ad[i] <- upperleftshot.twitch[i]/frac
  lowerleftshot.twitch.ad[i] <-lowerleftshot.twitch[i]/frac
  upperrightshot.twitch.ad[i] <-upperrightshot.twitch[i]/frac
  lowerrightshot.twitch.ad[i] <-lowerrightshot.twitch[i]/frac
}

boxplot <- data.frame(bodyshot.twitch.ad, upperleftshot.twitch.ad,lowerleftshot.twitch.ad, upperrightshot.twitch.ad,lowerrightshot.twitch.ad)

final_df <- as.data.frame(t(boxplot))
x <- c("Body Twitch", "Upper Left Twitch", "Lower Left Twitch", "Upper Right Twitch", "Lower Right Twitch")

k <- 200
len <- length(seq(0, T, k))
index <- seq(0, T, k)
viz_a_x <- matrix(0, nrow=14, ncol=len)
viz_a_y <- matrix(0, nrow=14,ncol=len)

viz_w_x <- matrix(0, nrow=14, ncol=len)
viz_w_y <- matrix(0, nrow=14, ncol=len)

viz_ul_x <- matrix(0, nrow=14, ncol=len)
viz_ul_y <- matrix(0, nrow=14, ncol=len)

viz_ll_x <- matrix(0, nrow=14, ncol=len)
viz_ll_y <- matrix(0, nrow=14, ncol=len)

viz_ur_x <- matrix(0, nrow=14, ncol=len)
viz_ur_y <- matrix(0, nrow=14, ncol=len)

viz_lr_x <- matrix(0, nrow=14, ncol =len)
viz_lr_y <- matrix(0, nrow=14, ncol=len)

for (s in 1:len) 
{
  i <- index[s] 
  
  if (i > 0)
  { 
    for(j in 1:13){
      viz_w_x[j,s] <- wax_x[i + (j-7)] - wax_x[i +(j-7)] + i
      viz_w_y[j,s] <- -wax_y[i+(j-7)]
      
      viz_a_x[j,s] <-  ab_x[i+(j-7)] - wax_x[i+(j-7)] + i
      viz_a_y[j,s] <- -ab_y[i+(j-7)]
      
      viz_ul_x[j,s] <-  left_knee_x[i+(j-7)] - wax_x[i+(j-7)] + i
      viz_ul_y[j,s] <- -left_knee_y[i+(j-7)]
      
      viz_ur_x[j,s] <-  right_knee_x[i+(j-7)] - wax_x[i+(j-7)] + i
      viz_ur_y[j,s] <- -right_knee_y[i+(j-7)]
      
      viz_lr_x[j,s] <-  right_foot_x[i+(j-7)] - wax_x[i+(j-7)] + i
      viz_lr_y[j,s] <- -right_foot_y[i+(j-7)]
      
      viz_ll_x[j,s] <-  left_foot_x[i+(j-7)] - wax_x[i+(j-7)] + i
      viz_ll_y[j,s] <- -left_foot_y[i +(j-7)]
    }
  }
  
  else{
    viz_w_x[,s] <- wax_x[1] - wax_x[1] + i
    viz_w_y[,s] <- -wax_y[1]
    
    viz_a_x[,s] <-  ab_x[1] - wax_x[1] + i
    viz_a_y[,s] <- -ab_y[1]
    
    viz_ul_x[,s] <-  left_knee_x[1] - wax_x[1] + i
    viz_ul_y[,s] <- -left_knee_y[1]
    
    viz_ur_x[,s] <-  right_knee_x[1] - wax_x[1] + i
    viz_ur_y[,s] <- -right_knee_y[1]
    
    viz_lr_x[,s] <-  right_foot_x[1] - wax_x[1] + i
    viz_lr_y[,s] <- -right_foot_y[1]
    
    viz_ll_x[,s] <-  left_foot_x[1] - wax_x[1] + i
    viz_ll_y[,s] <- -left_foot_y[1]
  }
  
  for(l in 1:len){
    viz_w_x[14,l] <- mean(viz_w_x[(1:13),l])
    viz_w_y[14,l] <- mean(viz_w_y[(1:13),l])
    
    viz_a_x[14,l] <-  mean(viz_a_x[(1:13),l])
    viz_a_y[14,l] <- mean(viz_a_y[(1:13),l])
    
    viz_ul_x[14,l] <-  mean(viz_ul_x[(1:13),l])
    viz_ul_y[14,l] <- mean(viz_ul_y[(1:13),l])
    
    viz_ur_x[14,l] <- mean(viz_ur_x[(1:13),l])
    viz_ur_y[14,l] <- mean(viz_ur_y[(1:13),l])
    
    viz_lr_x[14,l] <-  mean(viz_lr_x[(1:13),l])
    viz_lr_y[14,l] <- mean(viz_lr_y[(1:13),l])
    
    viz_ll_x[14,l] <-  mean(viz_ll_x[(1:13),l])
    viz_ll_y[14,l] <- mean(viz_ll_y[(1:13),l])
  }
}
# viz_x[6*s+1]<- viz_wax_x
# viz_x[6*s+2]<- viz_ab_x
# viz_x[6*s+3] <- viz_left_knee_x
# viz_x[6*s+4] <- viz_left_foot_x
# viz_x[6*s+5] <- viz_right_knee_x
# viz_x[6*s+6] <- viz_right_foot_x
# 
# viz_y[6*s+1]<- viz_wax_y
# viz_y[6*s+2]<- viz_ab_y
# viz_y[6*s+3] <- viz_left_knee_y
# viz_y[6*s+4] <- viz_left_foot_y
# viz_y[6*s+5] <- viz_right_knee_y
# viz_y[6*s+6] <- viz_right_foot_y

# viz_a_x[s] <- viz_ab_x
# viz_a_y[s] <- viz_ab_y
# 
# viz_w_x[s] <- viz_wax_x
# viz_w_y[s] <- viz_wax_y
# 
# viz_ul_x[s] <- viz_left_knee_x
# viz_ul_y[s] <- viz_left_knee_y
# 
# viz_ll_x[s] <- viz_left_foot_x
# viz_ll_y[s] <- viz_left_foot_y
# 
# viz_ur_x[s] <- viz_right_knee_x
# viz_ur_y[s] <- viz_right_knee_y
# 
# viz_lr_x[s] <- viz_right_foot_x
# viz_lr_y[s] <- viz_right_foot_y



### Magnitude of Angle Derivative ###
N <- length(angles) # 5732
angle.data <- matrix(0, nrow=N, ncol = 5) # (Time) x (Angle, Derivative, Smoothed Derivative, Second Derivative, Smoothed Second Derivative)
angle.data[,1] <- angles
angle.data[,2] <- rep(0, N)
angle.data[,3] <- rep(0, N)
angle.data[,4] <- rep(0, N)
angle.data[,5] <- rep(0, N)

# Eulerian Derivative calc
for(i in 1:(N-1))
{
  angle.data[i,2] <- ( angle.data[i+1, 1] - angle.data[i,1] )/( i+1 - i )
}

# Bulk data smooth calculation
for(i in 4:(N-3))
{
  k1 <- ( angle.data[i+1, 1] - angle.data[i,1] )/( i + 1 - i )
  k2 <- ( angle.data[i-1, 1] - angle.data[i,1] )/( i - 1 - i )
  k3 <- ( angle.data[i+2, 1] - angle.data[i,1] )/( i+2 - i )
  k4 <- ( angle.data[i-2, 1] - angle.data[i,1] )/( i-2 - i )
  k5 <- ( angle.data[i+3, 1] - angle.data[i,1] )/( i+3 - i )
  k6 <- ( angle.data[i-3, 1] - angle.data[i,1] )/( i-3 - i )
  angle.data[i, 3] <- (k1 + k2 + k3 + k4 + k5 + k6) / 6 # average
  
}


# Eulerian Second Derivative calc
for(i in 4:(N-3))
{
  angle.data[i,4] <- ( angle.data[i+1, 3] - angle.data[i,3] )/( i+1 - i )
}

# Bulk data Second Derivative smooth calculation
for(i in 7:(N-5))
{
  k1 <- ( angle.data[i+1, 4] - angle.data[i,4] )/( i + 1 - i )
  k2 <- ( angle.data[i-1, 4] - angle.data[i,4] )/( i - 1 - i )
  k3 <- ( angle.data[i+2, 4] - angle.data[i,4] )/( i+2 - i )
  k4 <- ( angle.data[i-2, 4] - angle.data[i,4] )/( i-2 - i )
  k5 <- ( angle.data[i+3, 4] - angle.data[i,4] )/( i+3 - i )
  k6 <- ( angle.data[i-3, 4] - angle.data[i,4] )/( i-3 - i )
  angle.data[i, 5] <- (k1 + k2 + k3 + k4 + k5 + k6) / 6 # average
  
}

## Boxplot Visualization of Response ##
dbvals <- rep(0, length(shots))

for(i in 1:length(shots)){
  dbvals[i] <- ss_x[shots[i]]
}

bplot <- matrix(0, nrow = 2, ncol = length(shots))
bplot[1,] <- dbvals
bplot[2,] <- abs(angle.frame) # OR should we separate based on side????

## Clumping Routine:
low <- rep(0,0)
mid <- rep(0,0)
high <- rep(0,0)
for(i in 1:length(shots)){
  if(dbvals[i] > 1 & dbvals[i] < 61){
    low <- c(low, bplot[2,i])
  } else if (dbvals[i] > 61 & dbvals[i] < 76){
    mid <- c(mid, bplot[2,i])
  } else if (dbvals[i] > 76 & dbvals[i] < 91){
    high <- c(high, bplot[2,i])
  }
}


pdf('Figures/191009_190708_ALTPlots.pdf', width=30,height=20)
par(mfrow = c(9,1), mar = c(.5,5,.5,.5), oma = c(8,2,4,24))
plot(1:frame_len, ss_x, type = "l", xlab = "Frame Number", ylab = "Sound", col = "midnightblue", cex = 1.5, cex.lab = 2, cex.axis = 2)
plot(shots, angle.frame, type = "l", xlab = "",ylab = "Angle Measurement", col = "mediumorchid4", cex = 1.5, cex.lab = 2, cex.axis = 1)
plot(x=seq(7:(frame_len - 5)), abs(angle.data[(7:(N-5)),3]), xlab = "Time", ylab = "|Angle Velocity|", ylim = c(0,3.5), type='l', cex = 1.5, cex.lab = 2, cex.axis = 1) # smooth derivative
par(new=TRUE)
plot(shots,bodyshot.twitch, type = "l", xaxt = "n",yaxt = "n", xlab="", ylab="", col = "blue", cex = 1.5, cex.lab = 2, cex.axis = 1)
axis(side=4)
par(new=FALSE)
plot(x=seq(7:(frame_len - 5)), abs(angle.data[(7:(N-5)),5]), xlab = "Time", ylab = "|Angle Acceleration|", type='l', cex = 1.5, cex.lab = 2, cex.axis = 1) # smooth Second derivative

plot(shots,lowerrightshot.twitch, type = "l", xlab = "", ylab = "Unnormalized Twitch", col = "red3", cex = 1.5, cex.lab=2, cex.axis = 1)
points(shots,lowerleftshot.twitch, type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2", cex = 1.5, cex.axis = 1)
points(shots,upperrightshot.twitch, type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2", cex = 1.5, cex.axis = 1)
points(shots,upperleftshot.twitch , type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green", cex = 1.5, cex.axis = 1)
plot(shots,bodyshot.twitch/max(bodyshot.twitch), type = "l", xlab = "", ylab = "Normalized Twitch", col = "blue", cex = 1.5, cex.lab = 2, cex.axis = 1)
points(shots,upperleftshot.twitch/max(upperleftshot.twitch), type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green", cex = 1.5, cex.axis = 1)
points(shots,lowerleftshot.twitch/max(lowerleftshot.twitch), type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2", cex = 1.5, cex.axis = 1)
points(shots,upperrightshot.twitch/max(upperrightshot.twitch), type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2", cex = 1.5, cex.axis = 1)
points(shots,lowerrightshot.twitch/max(lowerrightshot.twitch), type = "l", xlab = "", ylab = "Lower Right Twitch", col = "red3", cex = 1.5, cex.axis = 1)
#legend("right", xpd = TRUE, cex=1.7, pch=1, pt.cex = 1, legend = c("Body Twitch", "Upper Left Twitch", "Lower Left Twitch", "Upper Right Twitch", "Lower Right Twitch"), col = c("blue", "limegreen", "gold2", "darkorange2", "red3"),lty=1:1, lwd=4)
plot(shots,bodyshot.twitch.ad, type = "l", xlab = "", ylab = "Proportional Twitch", col = "blue", cex = 1.5, cex.lab = 2, cex.axis = 1)
points(shots,upperleftshot.twitch.ad, type = "l", xlab = "", ylab = "Upper Left Twitch", col = "lime green", cex = 1.5, cex.axis = 1)
points(shots,lowerleftshot.twitch.ad, type = "l", xlab = "", ylab = "Lower Left Twitch", col = "gold2", cex = 1.5, cex.axis = 1)
points(shots,upperrightshot.twitch.ad, type = "l", xlab = "", ylab = "Upper Right Twitch", col = "darkorange2", cex = 1.5, cex.axis = 1)
points(shots,lowerrightshot.twitch.ad, type = "l", xlab = "", ylab = "Lower Right Twitch", col = "red3", cex = 1.5, cex.axis = 1)
#barplot(as.matrix(final_df), col = c("blue", "limegreen", "gold2", "darkorange2", "red3"), xlab = "Shots", ylab= "Twitch Proportion", legend.text = TRUE, args.legend = list(x = "topright", bty = "n",inset=c(-0.7, 0)))

plot(shots, average.twitch, type = "l", xlab = "", ylab = "Average Twitch", cex = 1.5, cex.lab = 2, cex.axis = 1)
plot(viz_a_x[14,],viz_a_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "black",pch = 16, cex = 1.5, cex.lab = 2, cex.axis = 1, ylab = "Configurations")
points(viz_w_x[14,],viz_w_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "blue",pch = 16, cex = 1.5, cex.axis = 1)
points(viz_ul_x[14,],viz_ul_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "lime green",pch = 16, cex = 1.5, cex.axis = 1)
points(viz_ll_x[14,],viz_ll_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "gold2",pch = 16, cex = 1.5, cex.axis = 1)
points(viz_ur_x[14,],viz_ur_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "darkorange2",pch = 16, cex = 1.5, cex.axis = 1)
points(viz_lr_x[14,],viz_lr_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "red3",pch = 16, cex = 1.5, cex.axis = 1)
for(j in 1:len)
{
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_w_x[14,j],  viz_w_y[14,j], col = "grey", lwd = 2)
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_ul_x[14,j], viz_ul_y[14,j], col = "grey", lwd = 2)
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_ur_x[14,j], viz_ur_y[14,j], col = "grey", lwd = 2)
  segments(viz_ul_x[14,j], viz_ul_y[14,j], viz_ll_x[14,j], viz_ll_y[14,j], col = "grey", lwd = 2)
  segments(viz_ur_x[14,j], viz_ur_y[14,j], viz_lr_x[14,j], viz_lr_y[14,j], col = "grey", lwd = 2)
}
title(main = "191009_190708_ALT Graphs", sub = "Frame Number",cex.sub = 2.5,outer = TRUE, cex.main = 3)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", c("Body Twitch", "Upper Left Twitch", "Lower Left Twitch", "Upper Right Twitch", "Lower Right Twitch"), xpd = TRUE, horiz = FALSE, inset = c(0.007,0
), bty = "n", pch = 1, col = c("blue", "limegreen", "gold2", "darkorange2", "red3"), cex = 2, y.intersp = 5,lty=1:1, lwd=4)
# xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
# tells R that I want a horizontal legend inset = c(x,y) tells R how to move
# the legend relative to the 'bottom' location bty = 'n' means that 'no' box
# will be drawn around it pch and col are the types and colors of points cex
# = 2 makes the legend twice as large as other fonts
dev.off()
