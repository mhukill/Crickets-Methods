## 5_Grapher.R. Used with 1_Master.
setwd(output_directory) # enter subdirectory in which to store graphs
if(is.null(output_name)){ # generates the output name file according to preference specified in 1_Master.R
  output_name_pdf <- paste(file_name, "_graphs.pdf", sep='')
} else{
  output_name_pdf <- paste(output_name, ".pdf", sep='')
}

pdf(output_name_pdf, width=30,height=20)
par(mfrow = c(4,1), mar = c(0.5,7,0.5,0.5), oma = c(10,2,4,24),cex.lab=1.5,cex.axis=1.5)
plot(viz_a_x[14,],viz_a_y[14,], xlab = '', ylab='', ylim=c(-700,-100), col = "black",pch = 16,axes=FALSE, xaxt = 'n')
points(viz_w_x[14,],viz_w_y[14,], xlim = c(0, frame_len), ylim=c(-800,-100), col = "blue",pch = 16,cex=3)
points(viz_ul_x[14,],viz_ul_y[14,],col = "lime green",pch = 16,cex=3)
points(viz_ll_x[14,],viz_ll_y[14,],col = "gold2",pch = 16,cex=3)
points(viz_ur_x[14,],viz_ur_y[14,],col = "darkorange2",pch = 16,cex=3)
points(viz_lr_x[14,],viz_lr_y[14,],col = "red3",pch = 16,cex=3)
box()
text(x=(-75),y=(-650),label="(A)",cex=5)
mtext(side=2,"Mean Position", line=4,cex=2.5)
for(j in 1:len)
{
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_w_x[14,j],  viz_w_y[14,j], col = "grey", lwd = 4)
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_ul_x[14,j], viz_ul_y[14,j], col = "grey", lwd = 4)
  segments(viz_a_x[14,j], viz_a_y[14,j], viz_ur_x[14,j], viz_ur_y[14,j], col = "grey", lwd = 4)
  segments(viz_ul_x[14,j], viz_ul_y[14,j], viz_ll_x[14,j], viz_ll_y[14,j], col = "grey", lwd = 4)
  segments(viz_ur_x[14,j], viz_ur_y[14,j], viz_lr_x[14,j], viz_lr_y[14,j], col = "grey", lwd = 4)
}
plot(shots,lowerrightshot.twitch, type = "l", xlab = "", ylab = "", lwd = 4, col = rgb(205/255, 0, 0, 0.6),axes=FALSE)
points(shots,bodyshot.twitch, type = "l", col = rgb(0, 0, 1, 0.6), lwd = 4)
points(shots,lowerleftshot.twitch, type = "l", xlab = "", ylab = "Lower Left Twitch", col = rgb(238/255, 201/255, 0, 0.6), lwd = 4)
points(shots,upperrightshot.twitch, type = "l", xlab = "", ylab = "Upper Right Twitch", col = rgb(238/255, 118/255, 0, 0.6), lwd = 4)
points(shots,upperleftshot.twitch , type = "l", xlab = "", ylab = "Upper Left Twitch", col = rgb(50/255, 205/255, 50/255, 0.6), lwd = 4)
text(x=(-75),y=(4000),label="(B)",cex=5)
box()
mtext(side=2,expression(paste("Variation (",mm^2,")",sep="")), line=4,cex=2.5)
axis(2,at=c(10000,30000),labels=c(100,300),cex.axis=2.5)
plot(shots, angle.frame, type = "l", xlab = "",ylab = "", col = "mediumorchid4", lwd = 4,axes=FALSE)
mtext(side=2,expression(paste("Angle (degrees)",sep="")), line=4,cex=2.5)
text(x=(-75),y=(-2),label="(C)",cex=5)
box()
axis(2,at=seq(-2,10,by=2),labels=seq(-2,10,by=2),cex.axis=2.5)
cols <- rep("midnightblue",length(ss_x))

if(file_name_csv == "191009_190708_ALT.test.file_csv"){ ## this was used back when the decibel measurements weren't working properly: ignore it, don't delete it.
  ## BANDAID FIX FOR THE DECIBEL ISSUE
  # 5,5 | 4,5 | 5,5 | 5,5 | 5,5 | 5,5 | 5,5 | 5,5 | 5,4
  ud <- c(5,5,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4)
  out <- rep(0,0)
  
  light.green <- rgb(0.1,0.9,0.1,0.5)
  light.red <- rgb(0.9,0.1,0.1,0.5)
  
  for(r in 1:length(ud))
  {
    if (r%%2 ==1)
    {
      out <- c(out,rep(1,ud[r])) 
    }else
    {
      out <- c(out,rep(2,ud[r])) 
    }
  }
  
  counter <- 0
  for(j in 2:(length(ss_x)-1))
  {
    if ((ss_x[j]>0)&(ss_x[j-1]==0))
    {
      counter <- counter +1
    }
    if (ss_x[j]>0)
    {
      if(out[counter]==1)
      {
        cols[j] <- light.red
      }else
      {
        cols[j] <- light.green
      }
    }
  }
  
  par(mar=c(5,7,0.5,0.5))
  plot(1:frame_len,ss_x,  type = "h", xlab = "", ylab = "",pch=20, col = cols,axes=FALSE,ylim=c(0,100),cex=3,yaxs='i')
  axis(1,labels=TRUE,cex.axis=3,line=1,tick=FALSE)
  mtext(side=2,"Sound (dB)",line=4,cex=3)
  mtext(side=1,"Frame",line=4,cex=3)
  axis(2,labels=c(30,60,90),at=c(30,60,90),cex.axis=2.5)
  legend(x="topleft",legend=c("L","R"),title="Side",col=c(light.red,light.green),cex=5,pch=20)
  text(x=(-85),y=(10),label="(D)",cex=5)
  box()
} else{
  par(mar=c(5,7,0.5,0.5))
  plot(1:frame_len, ss_x, type='l', col = "seagreen", xlab="Frame", ylab='DLC Coord')
       # xlab = "", ylab = "", axes=FALSE, ylim=c(0,100), cex=3, yaxs='i')
  # axis(1,labels=TRUE,cex.axis=3,line=1,tick=FALSE)
  # mtext(side=2,"Sound (dB)",line=4,cex=3)
  # mtext(side=1,"Frame",line=4,cex=3)
  # axis(2,labels=c(30,60,90),at=c(30,60,90),cex.axis=2.5)
  # legend(x="topleft",legend=c("L","R"),title="Side",col=c(light.red,light.green),cex=5,pch=20)
  # text(x=(-85),y=(10),label="(D)",cex=5)
  box()
}


dev.off()

setwd(primary_directory) # return to main directory