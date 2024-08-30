# Best Case / Worst Case Code ---------------------------------------------

# Startup Code ------------------------------------------------------------
require(lolcat)
require(flextable)
require(dplyr)
require(ftExtra)

ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Inputs ------------------------------------------------------------------
xbar<-mean()
sd<-sd()
n<-length()
USL<-
LSL<-
CI<-
rnd<- # Number of digits to round to

# Test for Normality ------------------------------------------------------
summary.continuous()


# Confidence Interval for Mean --------------------------------------------
t.out<-t.test.onesample.simple(sample.mean = xbar
                        ,sample.variance = sd^2
                        ,sample.size = n
                        ,conf.level = CI)

(vec.mean<-ro(c(t.out$conf.int[1], xbar, t.out$conf.int[2]),rnd))

# Confidence Interval for Standard Deviation ------------------------------
var.out<-variance.test.onesample.simple(sample.variance = sd^2
                               ,sample.size = n
                               ,conf.level = CI)

(vec.sd<-ro(c(sqrt(var.out$conf.int[1]), sd, sqrt(var.out$conf.int[2])),rnd))

# Create dataframe of means and std. devs. --------------------------------
bcwc<-as.data.frame(vec.mean)
bcwc$sd<-vec.sd
names(bcwc)<-c("Mean", "SD")

# Upper sd, all means (lower to upper) ------------------------------------
uupsdmean<-pnorm(q = USL, mean = bcwc$Mean, sd = bcwc$SD[3], lower.tail = F)*100
lupsdmean<-pnorm(q = LSL, mean = bcwc$Mean, sd = bcwc$SD[3], lower.tail = T)*100

if(is.na(USL)) {
totrow1<-lupsdmean
} else if(is.na(LSL)) {
  totrow1<-uupsdmean
}  else {
  totrow1<-lupsdmean+ uupsdmean
  }

# Point Est sd, all means (lower to upper) --------------------------------
upsdmean<-pnorm(q = USL, mean = bcwc$Mean, sd = bcwc$SD[2], lower.tail = F)*100
lpsdmean<-pnorm(q = LSL, mean = bcwc$Mean, sd = bcwc$SD[2], lower.tail = T)*100

if(is.na(USL)) {
  totrow2<-lpsdmean
} else if(is.na(LSL)) {
  totrow2<-upsdmean
}  else {
  totrow2<-lpsdmean+upsdmean
}

# Lower sd, all means (lower to upper) ------------------------------------
ulowsdmean<-pnorm(q = USL, mean = bcwc$Mean, sd = bcwc$SD[1], lower.tail = F)*100
llowsdmean<-pnorm(q = LSL, mean = bcwc$Mean, sd = bcwc$SD[1], lower.tail = T)*100

if(is.na(USL)) {
  totrow3<-llowsdmean
} else if(is.na(LSL)) {
  totrow3<-ulowsdmean
}  else {
  totrow3<-llowsdmean+ulowsdmean
}

# Bind rows together ------------------------------------------------------
row1<-rbind(uupsdmean,lupsdmean, totrow1)
row2<-rbind(upsdmean, lpsdmean, totrow2)
row3<-rbind(ulowsdmean, llowsdmean, totrow3)

# Calculate Best and Worst Case -------------------------------------------
(min.out<-min(totrow1, totrow2, totrow3))
(max.out<-max(totrow1, totrow2, totrow3))

# Format Point and Interval Estimates for the table ---------------------
lclmean<-ro(t.out$conf.int[1],rnd)
pemean<-ro(xbar,rnd)
uclmean<-ro(t.out$conf.int[2],rnd)

lclsd<-ro(sqrt(var.out$conf.int[1]),rnd)
pesd<-ro(sd,rnd)
uclsd<-ro(sqrt(var.out$conf.int[2]),rnd)

# Create dataframe for table ----------------------------------------------
tab.out<-data.frame(round(rbind(row1, row2, row3),digits=9))
row.names(tab.out)<-c("1. % above USL", "2. % below LSL", "3. Total %", 
                      "4. % above USL", "5. % below LSL", "6. Total %", 
                      "7. % above USL", "8. % below LSL", "9. Total %")
tab.out$oos<-rownames(tab.out)
tab.out$sdest<-c(paste0("Upper Conf Limit \U03C3","\n",uclsd), paste0("Upper Conf Limit \U03C3","\n",uclsd), paste0("Upper Conf Limit \U03C3","\n",uclsd), 
                 paste0("Point Estimate \U03C3","\n",pesd), paste0("Point Estimate \U03C3","\n",pesd), paste0("Point Estimate \U03C3","\n",pesd),
                 paste0("Lower Conf Limit \U03C3","\n",lclsd), paste0("Lower Conf Limit \U03C3","\n",lclsd), paste0("Lower Conf Limit \U03C3","\n",lclsd))
tab.out<-tab.out[c(5,4,1,2,3)]
colnames(tab.out)<-c("Est. Pop. Std Dev","% out of spec",paste0("Lower Conf Limit \U00B5","\n",lclmean)
                     ,paste0("Point Estimate \U00B5","\n",pemean), paste0("Upper Conf Limit \U00B5","\n",uclmean))

# Find the max/min row/column index of the data portion of the tab --------
tdf <- tab.out
lkrows <- grep('*Total %', rownames(tdf))

maxind <- which(tdf[lkrows,c(3:5)] == max(tdf[lkrows,c(3:5)]), arr.ind = T)
maxind[1] <- maxind[1]*3;maxind[2] <- maxind[2]+2

minind <- which(tdf[lkrows,c(3:5)] == min(tdf[lkrows,c(3:5)]), arr.ind = T)
minind[1] <- minind[1]*3;minind[2] <- minind[2]+2

# Change first column to factor -------------------------------------------
tab.out$`Est. Pop. Std Dev`<-as.factor(tab.out$`Est. Pop. Std Dev`)
str(tab.out)

# Group by SD column ------------------------------------------------------
grouped_tab <- tab.out %>%
  group_by(`Est. Pop. Std Dev`)

# Format table using flextable package ------------------------------------
grouped_tab %>%
  as_flextable(groups_to = "merged") %>%
  autofit() %>%
  theme_box() %>%
  add_header_row(values = c("", "Est. Population Mean"), colwidths = c(2,3)) %>%
  align(align = "center", part = "header") %>% 
  align(i = NULL, j = 3:5, align = "center", part = "header") %>%
  align(i = NULL, j = 1, align = "center", part = "body") %>%
  bold(i = NULL, j = 1, bold = TRUE, part = "body") %>%
  align_nottext_col(align = "center") %>%
  colformat_double(digits = 2, suffix = "%") %>%
  bg(i = maxind[1], j = maxind[2], bg="red") %>%
  bg(i = minind[1], j = minind[2], bg="green")

