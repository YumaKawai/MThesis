## miで補完したデータを使って分析
## データの読み込み
IMP.1 <- read.csv("IMP.1.csv")
IMP.2 <- read.csv("IMP.2.csv")
IMP.3 <- read.csv("IMP.3.csv")

## データの修正
IMP.1$jud.ind[IMP.1$jud.ind == 4] <- 2
IMP.2$jud.ind[IMP.2$jud.ind == 4] <- 2
IMP.3$jud.ind[IMP.3$jud.ind == 4] <- 2

## 各データを使って分析
## 1
summary(IMP.1)
result.1 <- lm(physint ~ lag.physint + party1 + pop.size + gdp.us + jud.ind + type2 + type3
               + as.factor(cname) + as.factor(year), data = IMP.1)
summary(result.1)

## 2
summary(IMP.2)
result.2 <- lm(physint ~ lag.physint + party1 + pop.size + gdp.us + jud.ind + type2 + type3
               + as.factor(cname) + as.factor(year), data = IMP.2)
summary(result.2)

## 3
summary(IMP.3)
result.3 <- lm(physint ~ lag.physint + party1 + pop.size + gdp.us + jud.ind + type2 + type3
               + as.factor(cname) + as.factor(year), data = IMP.3)
summary(result.3)

## クラスター標準誤差を計算
clr(IMP.1, result.1, IMP.1$cname)
clr(IMP.2, result.2, IMP.2$cname)
clr(IMP.3, result.3, IMP.3$cname)

## 平均値モデルの推定
summary(MData)
result.mean <- lm(physint.imp.1 ~ lag.physint.imp.1 + party1.imp.1 + pop.size.imp.1 + gdp.us.imp.1
                  + jud.ind.imp.1 + type2 + type3 + as.factor(cname) + as.factor(year), data = MData)
summary(result.mean)

## クラスター標準誤差の計算
clr(MData, result.mean, MData$cname)
