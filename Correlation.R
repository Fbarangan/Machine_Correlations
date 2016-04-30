# Machine Correlation or Hematology Department
# Author : Felix Barangan, MS, RN, MLS(ASCP)

library(MethComp)
library(dplyr)
library(ggplot2)

getwd()

x <- read.csv("Instrument1.csv")
y <- read.csv("Instrument2.csv")

x_DF <- tbl_df(x)
y_DF <- tbl_df(y)


cnamesx <- names(x)
# add Instrument "X" to header to indicate instrument 1

for (i in seq_along(cnamesx)) {
        colnames(x_DF)[i] <- paste(cnamesx[i],"X", sep = "")
        colnames(x_DF)[1] = "sample"
}

# add Instrument "X" to header to indicate instrument 1

for (i in seq_along(cnamesx)) {
        colnames(y_DF)[i] <- paste(cnamesx[i],"Y", sep = "")
        colnames(y_DF)[1] = "sample"
}

mergeData_ <- merge(x_DF, y_DF, by = "sample")


g <- qplot( WBCX, WBCY, data = mergeData_ )

p  <- g + geom_point(color = "steelblue", size= 4, alpha = 1/2) + geom_smooth(method = "lm") + labs (title = paste(names(x)[2], "Correlation", sep = " ")) + labs(x = "Intrument1") + labs(y = "Instrument2")
print(p)

# Calculating for the Error Index
WBCStats <- cor.test(mergeData_$WBCX, mergeData_$WBCY)
WBCStats
WBCDdeming <- Deming(mergeData_$WBCX, mergeData_$WBCY)
WBCDdeming

WBCcoef <- coef(lm(WBCX ~ WBCY, data = mergeData_))

WBCcoef

mergeData_Bias <- mergeData_ %>%
         mutate(BiasWBC = WBCX - WBCY, WBCAveMeanDiff= (WBCX + WBCY)/2)

# Plot the error index

EI <- qplot(WBCAveMeanDiff, BiasWBC, data = mergeData_Bias)

ErrorIndex <- EI + geom_point(color = "steelblue", size = 4, alpha = 1/2)  +labs(title= "Error Index") + labs(x = paste(names(x)[2])) + labs(y = "Error Index (X-Y")

ErrorIndex
