#header <- scan("190624ADeepCut_resnet50_190708Jul8shuffle1_500000.csv", nlines = 1, what = character())
data <- read.csv("Data/Sound_Data_READY/191009_190708_ALT.test.file.csv", skip = 3, header = FALSE)

#Vectorizing the data
frame_col <- data[,1]
ab_x <- data[,2]
ab_y <- -data[,3]

frame_len <- length(ab_x) 

wax_x <- data[,5]
wax_y <- -data[,6]

ss_x <- data[, 8]
ss_y <- -data[,9]

left_knee_x <- data[,11]
left_knee_y <- -data[,12]

left_foot_x <- data[,14]
left_foot_y <- -data[,15]

right_knee_x <- data[, 17]
right_knee_y <- -data[, 18]

right_foot_x <- data[, 20]
right_foot_y <- -data[, 21]

#The following function allows us to make colors more transparent

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
# light_navy <- t_col("navy", 99)
# light_blue <- t_col('blue', 99)
# light_green <- t_col('green', 99)
# light_yellow <- t_col('yellow', 99)
# light_orange <- t_col('orange', 99)
# light_brown <- t_col('brown', 99)


#plot(1, col = "white",xlim = c(0,1000), ylim = c(-600, 0), xlab = "x-coordinates", ylab = "negative y-coordinates")

min_db <- min(ss_x)
max_db <- max(ss_x)

new_ss_x <- (((ss_x - min_db) * (0 - -60)) / (max_db - min_db)) + -60

col_ss_x <- (((ss_x - min_db) * (1 - 0)) / (max_db - min_db)) + 0
#sound <- rgb(0, 0, 255, max = 255, alpha = max(col_ss_x[start:end]) , names = "blue50")

cricket_graph <- function(start, end){
  
  plot(1,xlim = c(400,1000), ylim = c(-650, 0), axes=FALSE)
  #sound <- rgb(0, 0, 1, max = 1, alpha = max(col_ss_x[start:end]))
  #polygon(x = c(500, 550, 550, 500),y = c(-200, -200, -250, -250), col = sound)
  text(x = 450,y=-50,label=start,cex=2,col=grey(0.5))
  light_navy <- t_col("navy", 75)
  light_blue <- t_col('blue', 75)
  light_green <- t_col('green', 75)
  light_yellow <- t_col('yellow', 75)
  light_orange <- t_col('orange', 75)
  light_brown <- t_col('brown', 75)
  light_grey <- grey(0.7,0.01)
  
  
  points(ss_x[(start):(end)], ss_y[(start):(end)], col = "turquoise", pch = 16)
  
  points(ab_x[(start):(end)], ab_y[(start):(end)], col = light_navy, pch = 16)
  points(wax_x[(start):(end)],-wax_y[(start):(end)], col = light_blue, pch=16)
  points(left_knee_x[(start):(end)], left_knee_y[(start):(end)], col = light_green,pch = 16)
  points(left_foot_x[(start):(end)], left_foot_y[(start):(end)], col=light_yellow,pch = 16)
  points(right_knee_x[(start):(end)],right_knee_y[(start):(end)], col=light_orange,pch = 16)
  points(right_foot_x[(start):(end)], right_foot_y[(start):(end)], col=light_brown,pch = 16)
  
  for(j in start:(end-1)){
    segments(ab_x[j], ab_y[j], wax_x[j], wax_y[j], col = light_grey)
    segments(ab_x[j], ab_y[j], left_knee_x[j], left_knee_y[j], col = light_grey)
    segments(ab_x[j], ab_y[j], right_knee_x[j], right_knee_y[j], col = light_grey)
    segments(left_knee_x[j], left_knee_y[j], left_foot_x[j], left_foot_y[j], col = light_grey)
    segments(right_knee_x[j],right_knee_y[j], right_foot_x[j], right_foot_y[j], col = light_grey)
  }
  
  #segments(ab_x[end], -ab_y[end], wax_x[end], -wax_y[end], col = "red")
  #segments(ab_x[end], -ab_y[end], left_knee_x[end], -left_knee_y[end], col = "red")
  #segments(ab_x[end], -ab_y[end], right_knee_x[end], -right_knee_y[end], col = "red")
  #segments(left_knee_x[end], -left_knee_y[end], left_foot_x[end], -left_foot_y[end], col = "red")
  #segments(right_knee_x[end], -right_knee_y[end], right_foot_x[end], -right_foot_y[end], col = "red")
  
  ab_x_mean <- mean(ab_x[(start):(end)])
  ab_y_mean <- mean(ab_y[(start):(end)])
  wax_x_mean <- mean(wax_x[(start):(end)])
  wax_y_mean <- mean(wax_y[start:end])
  left_knee_x_mean <- mean(left_knee_x[start:end])
  left_knee_y_mean <- mean(left_knee_y[start:end])
  left_foot_x_mean <- mean(left_foot_x[start:end])
  left_foot_y_mean <- mean(left_foot_y[start:end])
  right_knee_x_mean <- mean(right_knee_x[start:end])
  right_knee_y_mean <- mean(right_knee_y[start:end])
  right_foot_x_mean <- mean(right_foot_x[start:end])
  right_foot_y_mean <- mean(right_foot_y[start:end])
  
  points(ab_x_mean, ab_y_mean, col = 'navy', pch=20)
  points(wax_x_mean, wax_y_mean, col='blue', pch=20)
  points(left_knee_x_mean, left_knee_y_mean, col = 'green', pch=20)
  points(left_foot_x_mean, left_foot_y_mean, col = 'yellow', pch=20)
  points(right_knee_x_mean, right_knee_y_mean, col = 'orange', pch=20)
  points(right_foot_x_mean, right_foot_y_mean, col = 'brown', pch=20)
  polygon(x = c(500, 550, 550, 500),y = c(200, 200, 250, 250))
  box()
  #  segments(ab_x_mean, -ab_y_mean, wax_x_mean, -wax_y_mean)
  # segments(ab_x_mean, -ab_y_mean, left_knee_x_mean, -left_knee_y_mean)
  # segments(ab_x_mean, -ab_y_mean, right_knee_x_mean, -right_knee_y_mean)
  # segments(left_knee_x_mean, -left_knee_y_mean, left_foot_x_mean, -left_foot_y_mean)
  #segments(right_knee_x_mean, -right_knee_y_mean, right_foot_x_mean, -right_foot_y_mean)
}

T=5262
library(jpeg)
img <- readJPEG("testpic.jpg")


pdf('Figures/191009_190708_ALTtestfile.pdf',width=30,height=20)
num.frames <- 277
par(mfrow=c(4,5),mar=c(0.1,0.1,0.1,0.1))
plot(1:10,ty="n", axes=FALSE, ylab="", xlab="")
rasterImage(img,0,0,10,10)
for(i in seq(0, T, num.frames)){
  cricket_graph(i, i+num.frames-1)
}
dev.off()

