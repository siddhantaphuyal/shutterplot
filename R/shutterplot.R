#' @title Shutter Plot
#' @description This function depicts the elements of a simple linear regression model.
#'
#' @param x data for the explanatory/independent variable.
#' @param y data for the response/dependent variable.
#' @param main the title for the shutter plot.
#' @param regbound logical: TRUE (Default), if you want the prediction boundaries;
#'     FALSE, otherwise.
#' @param wspace white space to the left and the right of the plot.
#'     The default is 0.1 (10 percent of the range of x).
#' @param alpha level of significance for prediction boundaries.
#'     The default value is 0.05 (97.5 percentile of a T-distribution with df = n-2.
#' @param locationOfnStar binary: -1 for left; 1 (Default) for right.
#' @param nprint logical: TRUE (Default), to print the sample size; FALSE, otherwise.
#' @param colOfPoints The default is "grey68". Choose any color.
#' @param xlab name of the x variable.
#' @param ylab name of the y variable.
#' @param regOutliers logical: TRUE (Default), to circle the regression outliers; FALSE, to skip.
#' @param las numeric in {0,1,2,3}; the style of axis labels.'
#'  0: always parallel to the axis [default],
#'  1:always horizontal,
#'  2:always perpendicular to the axis,
#'  3:always vertical.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default 0.7.
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#'            See points for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).
#'            The default value is 20.
#' @return Draws the shutter plot.
#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics abline curve lines par points rect segments text arrows
#' @importFrom stats cor lm median na.omit qt quantile sd cov var
#' @examples
#'   data1<- rnorm(90,10,10)
#'   data2<- data1+rnorm(90,20,10)
#'   shutterplot(data1,data2,regbound = TRUE,
#'      wspace = 0.1, alpha = 0.05,
#'      locationOfnStar = 1, nprint = TRUE, colOfPoints ="grey68",
#'       xlab = "data1", ylab = "data2", regOutliers = TRUE)
#'   shutterplot(data1,100-data2)

shutterplot<-function(x,y,main="Shutter Plot",regbound=TRUE,
                      wspace= 0.1,alpha = 0.05,locationOfnStar =1,
                      nprint = TRUE, colOfPoints="grey68",
                      xlab="x",ylab="y",regOutliers = TRUE,
                      pch =20, cex =0.7, las=1){

  oldpar<-par(no.readonly = TRUE)
  on.exit(par(oldpar))

  #omitting NA values from the given data
  creatingDataFrame<- data.frame(x=x,y=y)
  cleanDataFrame<- na.omit(creatingDataFrame)
  x<- cleanDataFrame$x
  y<- cleanDataFrame$y

  par(mgp = c(2,0.5,0))
  par(mar=c(3.5,3.5,1,1))
  mult<- c(2)
  r<- cor(x,y)
  t<- qt(1-alpha/2,length(x)-2)
  if(abs(cor(x,y))<0.1){
    cat ("WARNING: Shutter Plot is not meaningful when |r| < 0.1\nHere r = ",round(cor(x,y),4))
  }else{

    plot(x,y,
         xlab=xlab, ylab=ylab,las=las,main=main,
         ylim=c(min(y)-wspace*(max(y)-min(y)),max(y)+wspace*(max(y)-min(y))),
         xlim=c(min(x)-wspace*(max(x)-min(x)),max(x)+wspace*(max(x)-min(x))),
         frame.plot="F",pch=pch,cex=cex, col = colOfPoints,
         xaxs= "i",yaxs="i"
    )

    # regression line of y on x
    fit <- lm(y~x)
    yonxintercept <- fit$coefficient[[1]]
    yonxgradient <- fit$coefficient[[2]]
    abline(yonxintercept,yonxgradient)

    #regression line of x on y
    fit <- lm(x~y)
    xonygradient <- 1/fit$coefficient[[2]]
    xonyintercept <- (0-fit$coefficient[[1]])*xonygradient
    abline(xonyintercept,xonygradient, lty=3)

    #lines for the outliers box
    vec1<- c(mean(x)-mult*sd(x),mean(x)+mult*sd(x)
             ,mean(x)+mult*sd(x)
             ,mean(x)-mult*sd(x),mean(x)-mult*sd(x))
    vec2<- c(mean(y)-mult*sd(y),mean(y)-mult*sd(y),mean(y)+mult*sd(y)
             ,mean(y)+mult*sd(y)
             ,mean(y)-mult*sd(y))
    lines(vec1,vec2,lty=2, col="#7a7a7a")

    #horizontal line at mean of y
    meanofy<-mean(y)
    segments(x0=max(min(x),mean(x)-mult*sd(x)),y0=meanofy, x1= min(max(x),mean(x)+mult*sd(x)), y1 =meanofy,
             lty="dashed")
    text(quantile(x,0.75),meanofy,')')
    text(quantile(x,0.25),meanofy,'(')
    text(median(x),mean(y),'__',srt=90)
    segments(x0=quantile(x,0.25), y0 =meanofy,
             x1= quantile(x,0.75), y1=meanofy, lty="solid")
    text(min(max(x),mean(x)+mult*sd(x)),mean(y),'__',srt=90)
    text(max(max(min(x),mean(x)-mult*sd(x))),meanofy,'__',srt=90)

    #vertical line at mean of x
    meanofx<-mean(x)
    rangetwo <- ((max(x)-min(x))/4)*0.15
    segments(x0=meanofx,y0=max(min(y),mean(y)-mult*sd(y)), x1= meanofx,
             y1 =min(max(y),mean(y)+mult*sd(y)),
             lty="dashed")
    text(mean(x), median(y),'__')
    segments(x0=meanofx, y0 =quantile(y,0.25),
             x1=meanofx , y1=quantile(y,0.75), lty="solid")
    text(mean(x),max(min(y),mean(y)-mult*sd(y)),'__')
    text(mean(x),min(max(y),mean(y)+mult*sd(y)),'__')
    text(meanofx,quantile(y,0.75),')',srt=90)
    text(meanofx,quantile(y,0.25),'(',srt=90)




    #lines for the sd box
    vec3<- c(mean(x),min(max(x),mean(x)+sd(x))
             ,min(max(x),mean(x)+sd(x))
             ,mean(x),mean(x))
    vec4<- c(mean(y),mean(y),min(max(y),mean(y)+sign(cor(x,y))*sd(y))
             ,min(max(y),mean(y)+sign(cor(x,y))*sd(y))
             ,mean(y))
    lines(vec3,vec4,lty=1,col="#7a7a7a")

    #sd-line
    segments(x0 = mean(x)-mult*sign(cor(x,y))*sd(x),y0 = mean(y)-mult*sd(y),
             x1= mean(x)+mult*sign(cor(x,y))*sd(x),
             y1 =mean(y)+mult*sd(y),
             lty= 2)


    #smaller box for x
    segments(x0 = (mean(x)+r*sign(cor(x,y))*sd(x)),y0 = mean(y),
             x1=(mean(x)+r*sign(cor(x,y))*sd(x)),
             y1=min(max(y),mean(y)+sign(cor(x,y))*sd(y)),lty=1,col="#7a7a7a")

    #smaller box for y
    segments(x0 = mean(x),y0 = mean(y)+r*sd(y),
             x1=min(max(x),mean(x)+sd(x)),
             y1= mean(y)+r*sd(y), lty=1, col="#7a7a7a")

    #coloring the inner rectangle
    rect(xleft=mean(x),ybottom=mean(y),
         xright=(mean(x)+r*sign(cor(x,y))*sd(x)),ytop=mean(y)+r*(min(max(y),mean(y)+sd(y))-mean(y)),
         border =NA, col= rgb(186, 188, 191, maxColorValue = 255, alpha=76.5))

    #bold lines in the inner rectangle
    segments(x0 = (mean(x)+r*sign(cor(x,y))*sd(x)),y0 = mean(y),
             x1=(mean(x)+r*sign(cor(x,y))*sd(x)),
             y1=mean(y)+r*sign(cor(x,y))*r*(min(max(y),mean(y)+sd(y))-mean(y)),lty=1,lwd=2)
    segments(x0 = mean(x),y0 = mean(y)+r*sd(y),
             x1=mean(x)+r*r*(min(max(x),mean(x)+sd(x))-mean(x)),
             y1= mean(y)+r*sd(y), lty=1,lwd=2)


    #regboundaries
    sdofy<-sd(y)
    sdofx<-sd(x)
    lengthofx<-length(x)
    regbound1 <-function(x){
      return(meanofy+(r*(sdofy/sdofx)*(x-meanofx))+t*((1+(1/lengthofx)+((((x-meanofx)/sdofx)^2)/(lengthofx-1)))^0.5)*sdofy*((1-(r^2))^0.5))
    }
    regbound2 <-function(x){
      return(meanofy+(r*(sdofy/sdofx)*(x-meanofx))-t*((1+(1/lengthofx)+((((x-meanofx)/sdofx)^2)/(lengthofx-1)))^0.5)*sdofy*((1-(r^2))^0.5))
    }
    if(regbound == TRUE){
      curve(regbound1(x),  add=TRUE,type="l",col= "blue",lty=2)
      curve(regbound2(x),  add=TRUE,type="l",col= "blue",lty=2)
    }


    # for r outliers:
    upperlimit<-function(x,y){
      d<-mean(y)
      e<-(r*(sd(y)/sd(x))*(x-mean(x)))
      f<- t*((1+(1/length(x))+((((x-mean(x))/sd(x))^2)/(length(x)-1)))^0.5)*sd(y)*((1-(r*r))^0.5)
      return(d+e+f)
    }
    lowerlimit<-function(x,y){
      d<-mean(y)
      e<-(r*(sd(y)/sd(x))*(x-mean(x)))
      f<- t*((1+(1/length(x))+((((x-mean(x))/sd(x))^2)/(length(x)-1)))^0.5)*sd(y)*((1-(r*r))^0.5)
      return(d+e-f)
    }
    if((regbound == TRUE)&(regOutliers==TRUE)){
      points(x[(y>upperlimit(x,y))&(y<(mean(y)+mult*sd(y)))&(x>mean(x)-mult*sd(x))&(x< mean(x)+mult*sd(x))],y[(y>upperlimit(x,y))&(y<(mean(y)+mult*sd(y)))&(x>mean(x)-mult*sd(x))&(x<mean(x)+mult*sd(x))],pch = 1,col="red")
      points(x[(y<lowerlimit(x,y))&(y>(mean(y)-mult*sd(y)))&(x>mean(x)-mult*sd(x))&(x< mean(x)+mult*sd(x))],y[(y<lowerlimit(x,y))&(y>(mean(y)-mult*sd(y)))&(x>mean(x)-mult*sd(x))&(x<mean(x)+mult*sd(x))],pch = 1,col="red")
    }

    # for printing the number of samples:
    if (nprint==TRUE){
      text((mean(x)-sign(cor(x,y))*1.5*sd(x)),mean(y)+locationOfnStar*(0.5+mult)*sd(y),paste("( n* =",length(x),")"),col="brown")
    }

  }
}


#' @title Numerical Summaries of a Shutter Plot
#' @description displays numerical summaries of a shutter plot.
#' @param x data for the explanatory/independent variable.
#' @param y data for the response/dependent variable.
#' @param getValue logical:FALSE (DEFAULT); to access the summary statistics of the shutter plot.
#' @return Prints the numerical summaries in the console.
#' @export
#' @examples
#'   data1 <- rnorm(90,10,10)
#'   data2 <- data1 + rnorm(90,20,10)
#'   shutterplotsummary(data1,data2)



shutterplotsummary<-function(x,y,getValue=FALSE){
  creatingDataFrame<- data.frame(x=x,y=y)
  cleanDataFrame<- na.omit(creatingDataFrame)
  x<- cleanDataFrame$x
  y<- cleanDataFrame$y
  matrix1<-matrix(c(min(x),quantile(x,0.25),
                    median(x),mean(x),
                    quantile(x,0.75),max(x),sd(x)
                    ,length(x)),
                  nrow=1,ncol=8,byrow=TRUE)

  matrix2<-matrix(c(min(y),quantile(y,0.25)
                    ,median(y),mean(y),
                    quantile(y,0.75),max(y),sd(y)
                    ,length(y)),
                  nrow=1,ncol=8,byrow=TRUE)
  joinedmatrix <- rbind(matrix1,matrix2)
  colnames(joinedmatrix)<- c('Min.','1st Qu.', 'Median',
                             'Mean','3rd Qu.','Max.','sd','n')
  rownames(joinedmatrix)<-c('x','y')
  if(getValue==TRUE){
    return(joinedmatrix)
  }
  print (joinedmatrix)

  matrix3<-matrix(c(var(x),cov(x,y),
                    cov(x,y),var(y))
                  ,nrow =2 ,ncol=2, byrow=TRUE)
  colnames(matrix3) <- c('x','y')
  rownames(matrix3)<- c('x','y')
  cat("\nCovariance Matrix\n")
  print (matrix3)
  cat("\nCorrelation ",round(cor(x,y),4),"\n")

}


#' @title seven-number-summary
#' @description displays the seven-number-summary for a variable.
#'
#' @param x value(s) of a variable.
#'
#' @importFrom grDevices dev.new
#' @importFrom graphics axis
#' @importFrom graphics plot
#' @return depicts the seven-number-summary.
#' @export
#' @examples
#'   data<- rnorm(90,90,10)
#'   summary7plot(data)

summary7plot <- function(x){
  h<- 1.95
  x<-na.omit(x)
  y<-rep(h,length(x))
  plot(x,y, main=paste("Seven Number Plot","\n","(non-missing n=",length(x),")"),
       yaxt="none", xlab="", ylab="", type="n",
       ylim=c(0,2),xlim=c(min(x),(max(x)*5-min(x))/4),
       frame.plot="F",axes=FALSE)
  x1 <- as.integer(min(x))
  x2 <- as.integer(max(x))
  axis(1, pos=1.7,at=c(seq(x1-2,x2+2,2)), padj=0,cex=1,xpd=TRUE) # CHANGE VALUE
  arrows(x1-2,1.7,x2+2,1.7, code = 2, xpd = TRUE, length=.10)
  segments(x0=min(x),y0=h, x1= max(x), y1 =h, lty="dashed")
  segments(x0=quantile(x,0.25), y0 =h, x1= quantile(x,0.75), y1=h, lty="solid")
  text(quantile(x,0.75),h,')')
  text(quantile(x,0.25),h,'(')
  text(min(x),h,'|')
  text(max(x),h,'|')
  text(median(x),h,'|')
  sdeviation = sqrt(var(x))
  finalposition <- mean(x)+ sdeviation
  arrows(x0=mean(x),y0=h,x1=finalposition,y1=h, length="0.15", lty="solid", lwd=2.8)
}

#' @title Numerical values of seven-number-summary.
#' @description prints the numerical summaries in the console.
#' @param x value(s) of a variable.
#' @return prints the seven-number-summary in the console.
#' @export
#' @examples
#'   data <- rnorm(90,90,10)
#'   summary7(data)

summary7 <- function(x){
  x<-na.omit(x)
  y<-matrix(c(min(x),quantile(x,0.25)
              ,median(x),mean(x),
              quantile(x,0.75),max(x),sd(x)
              ,length(x)),
            nrow=1,ncol=8,byrow=TRUE)
  colnames(y)<- c('Min.','1st Qu.', 'Median','Mean',
                  '3rd Qu.','Max.','sd','n')
  rownames(y)<-c('')
  y
}


