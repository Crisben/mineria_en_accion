setwd("/home/mario/mineria_en_accion/regression/moneyball")

# Part 1

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
(95-80.881375)/0.105766
#134


# Part 2

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
#OBP = 0.339, SLG = 0.430
-804.63 + 2737.77*0.339 + 1584.91*0.430
#805

RunsAll = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAll)
#OOBP = 0.307, OSLG = 0.373
-837.38 + 2913.60*0.307 + 1514.29*0.373
#622

#Prediction of the wins
80.881375 + 0.105766*(805-622)
#100

