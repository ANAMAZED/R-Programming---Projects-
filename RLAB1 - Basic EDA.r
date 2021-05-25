mydata = read.csv("C:/Users/anama/Downloads/Auto.csv")

  > library(ggplot)
Error in library(ggplot) : there is no package called 'ggplot'
> library(ggplot2)
> 
  > ggplot(Auto.csv, aes(x= cylinders, y = mpg)) + geom_boxplot()
Error in ggplot(Auto.csv, aes(x = cylinders, y = mpg)) : 
  object 'Auto.csv' not found
> ggplot(mydata, aes(x= cylinders, y= mpg)) +geom_boxplot()
Warning message:
  Continuous x aesthetic -- did you forget aes(group=...)? 
  >
Restarting R session...


  > str(mydata)
'data.frame':	397 obs. of  9 variables:
  $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
$ cylinders   : int  8 8 8 8 8 8 8 8 8 8 ...
$ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
$ horsepower  : int  130 165 150 150 140 198 220 215 225 190 ...
$ weight      : int  3504 3693 3436 3433 3449 4341 4354 4312 4425 3850 ...
$ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
$ year        : int  70 70 70 70 70 70 70 70 70 70 ...
$ origin      : int  1 1 1 1 1 1 1 1 1 1 ...
$ name        : chr  "chevrolet chevelle malibu" "buick skylark 320" "plymouth satellite" "amc rebel sst" ...
> 
  > library(ggplot2)
> 
  https://cran.rstudio.com/bin/windows/Rtools/
  Installing package into 'C:/Users/anama/OneDrive/Documents/R/win-library/4.0'
(as 'lib' is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.0/tidyverse_1.3.0.zip'
Content type 'application/zip' length 440017 bytes (429 KB)
downloaded 429 KB

package 'tidyverse' successfully unpacked and MD5 sums checked

The downloaded binary packages are in
C:\Users\anama\AppData\Local\Temp\RtmpMJ4mZM\downloaded_packages
> install.packages("pacman")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:
  
  https://cran.rstudio.com/bin/windows/Rtools/
  Installing package into 'C:/Users/anama/OneDrive/Documents/R/win-library/4.0'
(as 'lib' is unspecified)
also installing the dependency 'remotes'

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.0/remotes_2.2.0.zip'
Content type 'application/zip' length 388631 bytes (379 KB)
downloaded 379 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.0/pacman_0.5.1.zip'
Content type 'application/zip' length 389890 bytes (380 KB)
downloaded 380 KB

package 'remotes' successfully unpacked and MD5 sums checked
package 'pacman' successfully unpacked and MD5 sums checked

The downloaded binary packages are in
C:\Users\anama\AppData\Local\Temp\RtmpMJ4mZM\downloaded_packages
> qplot(mpg, displacement, data = mydata)
> qplot(mpg, horsepower, data = mydata)
Warning message:
  Removed 5 rows containing missing values (geom_point). 
> 
                               > boxplot(mydata$mpg ~ mydata$cylinders)
                               > 
                                 > 
                               > qplot(mydata$horsepower, mydata$mpg)
                               Warning message:
                                 
                                 > plot(mydata$horsepower, mydata$weight)
                            
                                 
                                 > plot(mydata$horsepower, mydata$weight)
                               > identify(mydata$horsepower, mydata$weight, mydata$name)
                               warning: nearest point already identified
                               warning: nearest point already identified
                               warning: nearest point already identified
                               [1] 14
                               > 
                                 > 
                                 > plot(mydata$horsepower, mydata$mpg)
                               > identify(mydata$horsepower, mydata$mpg, mydata$name)
                               warning: nearest point already identified
                               [1] 334