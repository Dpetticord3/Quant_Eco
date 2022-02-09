
#Lab 3: Statistical Inference

#Bootstrap: 
library(readxl)
library(boot)
bci_census <- read_excel("data/bci_census.xlsx")

head(bci_census)
dbh = na.omit((as.numeric(bci_census$dbh)))

median_population = median(dbh)
#we are adding a few conditions onto this as we did before 
#(treat dbh as a numeric variable & omit the necessary nas)
print(median_population)

#we want to sample randomly without replacement
set.seed(123)
nrow(bci_census)
tenth = nrow(bci_census)*0.1
df_sample = sample(dbh, size = tenth)

str(df_sample)                               
summary(df_sample)

#bootstrapped median and Ci

#we can do this in two steps


boot_Median = function(data, i) median(data[i])

b = boot(df_sample, boot_Median, R = 1000)
print(b)

#could also do something like this: 
#number of reps
B = 10000
#empty storage vectr
result_vec = vector(length=B)


for(b in 1:B) {
  # draw a bootstrap sample
  this_sample <- sample(dbh, size=tenth, replace=TRUE)
  
  # calculate your statistic
  m = median(this_sample)
  
  # save your calucated statistic
  result_vec[b] <- m
}

# then  draw a histogram of your bootstrapped replicates
hist(result_vec)

# get 95% confidence interval
result_vec <- result_vec[order(result_vec)]
lower_bound <- result_vec[round(0.025*B)]
upper_bound <- result_vec[round(0.0975*B)]

print(result_vec)
print(lower_bound)
print(upper_bound)
###########################################
#quick showcase of how read_excel can  name your columns at your will
Powers_Tiffin_2010 <- read_excel("data/Powers_Tiffin_2010.xls", col_names = FALSE, col_types = c("text","text", "text", "text", "skip", "text","text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

colnames(Powers_Tiffin_2010) = c('order', 'family', 'genus', 'species', 'leaf_habitat',
              'leaf_type', 'SLA', 'WD','LWC','d15N','d13C', 'leaf_P', 'leaf_N','leaf_C', 'leaf_C2N', 'leaf_N2P')
#this still keeps our original headers as the first row, so lets remove it
Powers_Tiffin_2010 = Powers_Tiffin_2010[-1,]
head(Powers_Tiffin_2010)

#lets do some elementary stats. R is *really* good at this
#first lets make sure the interesting variable is in the right form
library(ggpubr)
ggqqplot(Powers_Tiffin_2010$d13C)

#we can also do a variety of normality tests fairly simply
d13c_scaled = scale(Powers_Tiffin_2010$d13C)

shapiro.test(d13c_scaled)
ks.test(d13c_scaled, dnorm)

#lets look at the  histograms: 
library(tidyverse)

#we can also use the gridextra package to manipulate these and put them in a nicer side by side
library(gridExtra)

decid = Powers_Tiffin_2010 %>%
  filter(leaf_habitat == "deciduous")

nd = Powers_Tiffin_2010 %>%
  filter(leaf_habitat != 'deciduous')

g1 = gghistogram(decid, x = "d13C", y = "..density..", add = "mean", rug = TRUE, main = "deciduous trees", fill = "red")

g2 = gghistogram(nd, x = "d13C",  y = "..density..", add = "mean", main = "non-deciduous trees", rug = TRUE, fill = "blue") 

grid.arrange(g1,g2, ncol =2)

ks.test(decid$d13C, nd$d13C)

########################
#distribution of trait values in deciduous vs. Evergreen
#to apply some function (shapiro.test) across a df we use apply

head(decid)
head(nd)

#first lets go ahead and define our traits
traits = c('SLA','WD','leaf_C2N','leaf_N2P','d13C')

#we can then use apply
decid.shapiro = apply(decid[,traits],2, shapiro.test)

#and now we can extract p values like this: 
unlist(lapply(decid.shapiro, function(x) x$p.value))

#and again for the non-deciduous is the same process:
nd.shapiro = apply(nd[,traits],2, shapiro.test)
unlist(lapply(nd.shapiro, function(x) x$p.value))

#our results are different? Why? It is because of the decision to use deciduous vs. evergreen without including our third group, semi-d/

#we can see that we use the Bartlett test on SLA and WD, but the Levene on the rest

bartlett.test(SLA ~ leaf_habitat, data = Powers_Tiffin_2010)
#because this p value is greater than 0.05, we know that there is no evidence to 
#suggest that the variance in SLA is statistically diff for the two leaf habitats

#the leveneTest in R lives in the car package, which we call here
library(car)
result = leveneTest(leaf_C2N ~leaf_habitat, data = Powers_Tiffin_2010)
print(result)
#########################################################
#T-test's and checking population mean.
ref = 26.5

t.test(Powers_Tiffin_2010$d13C, mu = 26.5, alternative = "two.sided")

t.test(Powers_Tiffin_2010$d13C, mu = 26.5, alternative = "greater")
t.test(Powers_Tiffin_2010$d13C, mu = 26.5, alternative = "less")

#################
#query, do legume's store more nitrogen in leaves? 
#legumes
legume = Powers_Tiffin_2010 %>%
  filter(Powers_Tiffin_2010$family == "Fabaceae")

nonlegume = Powers_Tiffin_2010 %>%
  filter(Powers_Tiffin_2010$family != "Fabaceae")


Leg_N = legume$leaf_N
nLeg_N = nonlegume$leaf_N

print(shapiro.test(Leg_N))
print(shapiro.test(nLeg_N))

gL =ggqqplot(Leg_N, main = "Legume leaf N")
gNL = ggqqplot(nLeg_N, main = "non-Legume leaf N")

grid.arrange(gL, gNL, ncol = 2)

#parametric T-test
t.test(Leg_N, nLeg_N)
#non-parametric mann-whitney
wilcox.test(Leg_N,nLeg_N)
#########################################


#reproduce Tukey test figure 
#install.packages("multcompView")
library(multcompView)

#ok so first step is adding legume as a leaf habitat, I guess
legume = Powers_Tiffin_2010 %>%
  filter(Powers_Tiffin_2010$family == "Fabaceae")
legume$leaf_habitat = "legume"

nonlegume = Powers_Tiffin_2010 %>%
  filter(Powers_Tiffin_2010$family != "Fabaceae")

Figure = rbind(legume,nonlegume)


par(mfrow = c(5,2))

#SLA

SLA_model = lm(Figure$SLA~Figure$leaf_habitat)
Anova_SLA = aov(SLA_model)
TUKEY = TukeyHSD(x = Anova_SLA, 'Figure$leaf_habitat', conf.level = 0.95)
generate_label_df = function(TUKEY, variable){
  
  #extract levels and labels
  Tukey.levels = TUKEY[[variable]][,4]
  Tukey.labels = data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #add labels in the order of the boxplot
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
  
}
#apply this function to each dataset: 
LABELS <- generate_label_df(TUKEY , "Figure$leaf_habitat")
over <- 0.1*max( a$stats[nrow(a$stats),] )

# Draw the basic boxplot
boxplot(Figure$SLA ~ Figure$leaf_habitat, ylab="value" , main="SLA")
#text(c(1:4) , 175, LABELS[,1])
text(x =2, y = 200, print(paste0("pvalue =",round(summary(Anova_SLA)[[1]][1,5],3))))
text(x=3, y = 200, print(paste0("F-value=",round(summary(Anova_SLA)[[1]][1,4],3))))

########################################################################################


#WD

WD_model = lm(Figure$WD~Figure$leaf_habitat)
Anova_WD = aov(WD_model)
TUKEY = TukeyHSD(x = Anova_WD, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")
over <- 0.1*max( a$stats[nrow(a$stats),] )


boxplot(Figure$WD ~ Figure$leaf_habitat, ylab="value" ,ylim= c(0.0,1.2), main="WD")
text(c(1:4) ,0.8, LABELS[,1])
text(x =3, y = 1.1, print(paste0("pvalue =",round(summary(Anova_WD)[[1]][1,5],3))))
text(x=2, y = 1.1, print(paste0("F-value=",round(summary(Anova_WD)[[1]][1,4],3))))

#LWC
LWC_model = lm(Figure$LWC~Figure$leaf_habitat)
Anova_LWC = aov(LWC_model)
TUKEY = TukeyHSD(x = Anova_LWC, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")

boxplot(Figure$LWC ~ Figure$leaf_habitat, ylab="value" ,ylim= c(40,80), main="LWC")
#text(c(1:4) , 75, LABELS[,1])
text(x =3, y = 80, print(paste0("pvalue =",round(summary(Anova_LWC)[[1]][1,5],3))))
text(x=2, y = 80, print(paste0("F-value=",round(summary(Anova_LWC)[[1]][1,4],3))))

#d13C
d13C_model = lm(Figure$d13C~Figure$leaf_habitat)
Anova_d13C = aov(d13C_model)
TUKEY = TukeyHSD(x = Anova_d13C, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")

boxplot(Figure$d13C ~ Figure$leaf_habitat, ylab="value" ,ylim= c(-34,-26), main="d13C")
over <- 0.05*max( a$stats[nrow(a$stats),] )
text(c(1:4) ,y = -26, LABELS[,1])
text(x =3, y = -34, print(paste0("pvalue =",round(summary(Anova_d13C)[[1]][1,5],3))))
text(x=2, y = -34, print(paste0("F-value=",round(summary(Anova_d13C)[[1]][1,4],3))))
#d15N
d15N_model = lm(Figure$d15N~Figure$leaf_habitat)
Anova_d15N = aov(d15N_model)
TUKEY = TukeyHSD(x = Anova_d15N, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")

boxplot(Figure$d15N ~ Figure$leaf_habitat, ylab="value" ,ylim= c(-4,8), main="d15N")
#text(c(1:4) ,y = 6, LABELS[,1])
text(x =3, y = 8, print(paste0("pvalue =",round(summary(Anova_d15N)[[1]][1,5],3))))
text(x=2, y = 8, print(paste0("F-value=",round(summary(Anova_d15N)[[1]][1,4],3))))


#LeafC
leaf_C_model = lm(Figure$leaf_C~Figure$leaf_habitat)
Anova_leaf_C = aov(leaf_C_model)
TUKEY = TukeyHSD(x = Anova_leaf_C, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")



boxplot(Figure$leaf_C ~ Figure$leaf_habitat, ylab="value" ,ylim= c(35,55), main="leaf_C")
over <- 0.05*max( a$stats[nrow(a$stats),] )
text(c(1:4) ,y = 36, LABELS[,1])
text(x =3, y = 55, print(paste0("pvalue =",round(summary(Anova_leaf_C)[[1]][1,5],3))))
text(x=2, y = 55, print(paste0("F-value=",round(summary(Anova_leaf_C)[[1]][1,4],3))))

#LeafN
leaf_N_model = lm(Figure$leaf_N~Figure$leaf_habitat)
Anova_leaf_N = aov(leaf_N_model)
TUKEY = TukeyHSD(x = Anova_leaf_N, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")


boxplot(Figure$leaf_N ~ Figure$leaf_habitat, ylab="value" ,ylim= c(1,4), main="leaf_N")
over <- 0.05*max( a$stats[nrow(a$stats),] )
text(c(1:4) ,y = 1, LABELS[,1])
text(x =3, y = 3.9, print(paste0("pvalue =",round(summary(Anova_leaf_N)[[1]][1,5],3))))
text(x=2, y = 3.9, print(paste0("F-value=",round(summary(Anova_leaf_N)[[1]][1,4],3))))

#LeafP
leaf_P_model = lm(Figure$leaf_P~Figure$leaf_habitat)
Anova_leaf_P = aov(leaf_P_model)
TUKEY = TukeyHSD(x = Anova_leaf_P, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")


boxplot(Figure$leaf_P ~ Figure$leaf_habitat, ylab="value" ,ylim= c(0.05,0.20), main="leaf_P")
over <- 0.05*max( a$stats[nrow(a$stats),] )
#text(c(1:4) ,y = 0.1, LABELS[,1])
text(x =3, y = 0.3, print(paste0("pvalue =",round(summary(Anova_leaf_P)[[1]][1,5],3))))
text(x=2, y = 0.3, print(paste0("F-value=",round(summary(Anova_leaf_P)[[1]][1,4],3))))

#LeafCN
leaf_C2N_model = lm(Figure$leaf_C2N~Figure$leaf_habitat)
Anova_leaf_C2N = aov(leaf_C2N_model)
TUKEY = TukeyHSD(x = Anova_leaf_C2N, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")

boxplot(Figure$leaf_C2N ~ Figure$leaf_habitat, ylab="value" ,ylim= c(10,50), main="leaf_C2N")
text(c(1:4) ,y = 10, LABELS[,1])
text(x =3, y = 50, print(paste0("pvalue =",round(summary(Anova_leaf_C2N)[[1]][1,5],3))))
text(x=2, y = 50, print(paste0("F-value=",round(summary(Anova_leaf_C2N)[[1]][1,4],3))))

#LeafNP
leaf_N2P_model = lm(Figure$leaf_N2P~Figure$leaf_habitat)
Anova_leaf_N2P = aov(leaf_N2P_model)
TUKEY = TukeyHSD(x = Anova_leaf_N2P, 'Figure$leaf_habitat', conf.level = 0.95)

LABELS <- generate_label_df(TUKEY, "Figure$leaf_habitat")
boxplot(Figure$leaf_N2P ~ Figure$leaf_habitat, ylab="value" ,ylim= c(10,50), main="leaf_N2P")
text(c(1:4) ,y = 10, LABELS[,1])
text(x =3, y = 50, print(paste0("pvalue =",round(summary(Anova_leaf_N2P)[[1]][1,5],3))))
text(x=2, y = 50, print(paste0("F-value=",round(summary(Anova_leaf_N2P)[[1]][1,4],3))))

######################################################################

#correlation plot in R
#restore the plotting window
par(mfrow=c(1,1))


#baseR 
my_data = Powers_Tiffin_2010[,7:16]
res = cor(my_data)
round(res,2)
#using hmisc
library(Hmisc)

#for flattening hmisc object
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#check correlations
res2<-rcorr(as.matrix(Powers_Tiffin_2010[,7:16]))
#print correlation matrix
flattenCorrMatrix(res2$r, res2$P)
#extract P values
res2$P
#extract correlation coefficients
res2$r


#visualize
#The R function symnum() replaces correlation coefficients by symbols according 
#to the level of the correlation. It takes the correlation matrix as an argumeny:

symnum(res, abbr.colnames = FALSE)

#with corrplot
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#with pairs & custom functions: 
pairs(res, pch = 19, upper.panel = NULL)

#to add a correlation coefficient
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}
#to add histograms 
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

pairs(res, upper.panel = panel.cor, diag.panel  = panel.hist,lower.panel = panel.smooth)

#Mock up of the fig from the paper 
lower.panel<-function(x, y){
  points(x,y, pch=19)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(res, lower.panel = lower.panel, upper.panel = NULL)
