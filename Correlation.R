# Machine Correlation or Hematology Department
# Author : Felix Barangan, MS, RN, MLS(ASCP)

install.packages("broman")
install.packages("dplyr")
install.packages("stats")

library(dplyr)
library(ggplot2)
library(stats)


getwd()

x <- read.csv("Instrument1.csv")
y <- read.csv("Instrument2.csv")

x_DF <- tbl_df(x)
y_DF <- tbl_df(y)

cv <- function(mean,sd){
        (sd/mean) *100
}


cnamesx <- names(x)
# add Instrument "X" to header to indicate instrument 1

for (i in seq_along(cnamesx)) {
        colnames(x_DF)[i] <- paste(cnamesx[i],"Intrument1", sep = "")
        colnames(x_DF)[1] = "sample"
}

# add Instrument "X" to header to indicate instrument 1

for (i in seq_along(cnamesx)) {
        colnames(y_DF)[i] <- paste(cnamesx[i],"Instrument2", sep = ".")
        colnames(y_DF)[1] = "sample"
}

mergeData_ <- merge(x_DF, y_DF, by = "sample")

# Calculating Error index
# will need to consider this 0.15 as possible user input
EI <- mergeData_ %>%
        mutate(Error_Index = round(((WBC.Instrument2 - WBCIntrument1)/ WBCIntrument1) / 0.15, 3),
               aveEI_Vector_ = round(ave(as.vector(as.vector(Error_Index))),3),
               biasWBC = WBC.Instrument2 - WBCIntrument1,
               actualpercentBias =  (biasWBC / WBCIntrument1)*100,
               aveActualpercentBias = mean(actualpercentBias))


EI_Vector_ <- as.vector(as.vector(EI$Error_Index))
ave_EI <- sum(EI_Vector_)/ length(EI_Vector_)
abs.ave_EI <-  abs(ave_EI)
###-----


#Code for bias y-x

biasWBC_Vector <- as.vector(as.vector(EI$biasWBC))
ave_biasWBC_Vector <- sum(biasWBC_Vector)/ length(biasWBC_Vector)
abs.ave_biasWBC_Vector <-  abs(ave_biasWBC_Vector)



###---- linear plots between Instrument1 and Instrument 2-----

g <- qplot( WBCIntrument1, WBC.Instrument2, data = mergeData_ )

p  <- g + geom_point(color = "steelblue", size= 4, alpha = 1/2) + geom_smooth(method = "lm") + labs (title = paste(names(x)[2], "Correlation", sep = " ")) + labs(x = "Intrument 1") + labs(y = "Instrument 2")
print(p)

##

##---Start Average of Analyte in question----
meanEIbiasWBC <- mean(EI$biasWBC)
sdEIbiasWBC <- sd(EI$biasWBC)

cvWBC.Instrument2 <- cv(mean = meanEIbiasWBC, sd=sdEIbiasWBC)
abs.cvWBC.Instrument2 <- abs(cvWBC.Instrument2)


##---End------

sigmaDecisionChart <- (15 - abs.ave_biasWBC_Vector) / cvWBC.Instrument2


# Calculating for the Error Index
WBCStats <- cor.test(mergeData_$WBCIntrument1, mergeData_$WBC.Instrument2)
WBCStats
WBCDdeming <- Deming(mergeData_$WBCIntrument1, mergeData_$WBC.Instrument2)
WBCDdeming

WBCcoef <- coef(lm(WBCIntrument1 ~ WBC.Instrument2, data = mergeData_))

WBCcoef


# Plot the Error_index

EIplot <- qplot(WBCIntrument1, Error_Index, data = EI)

ErrorIndexplot <- EIplot + geom_point(color = "steelblue", size = 4, alpha = 1/2)  +labs(title= "Error Index") + geom_hline(yintercept = EI$aveEI_Vector_ ) + labs(x = paste(names(x)[2])) + labs(y = "Error Index (X-Y")

print(ErrorIndexplot)

# Plot actual  x-y bias to represent the 15 % TEa for WBC
upperLimitTEa <- 15
lowerLimitTea <- -15


actualBiasWithRange <- qplot(WBCIntrument1, actualpercentBias, data = EI)

actualBiasWithRangePlot <- actualBiasWithRange + geom_point(color = "red", size = 2, alpha = 1/2)  +labs(title= "Actual Bias with TEa Range") + geom_hline(yintercept = upperLimitTEa, size=1 ) + geom_hline(yintercept = lowerLimitTea, size = 1 ) + geom_hline(yintercept = EI$aveActualpercentBias, color = "steelblue" ) + labs(x = paste(names(x)[2])) + labs(y = "Actual Bias(Y-X")

print(actualBiasWithRangePlot)

# Data
showWBC <- EI %>%
        select(sample, WBCIntrument1,  WBC.Instrument2,  biasWBC, actualpercentBias, Error_Index)

# 166 cv
#
#Reference
## https://www.aacc.org/publications/cln/articles/2013/september/total-analytic-error
#3 Notes = Sigma MEtric = (%TEa - % Bias)/ %CV
# https://www.westgard.com/clia.htm
# CV = (sd/mean) * 100
