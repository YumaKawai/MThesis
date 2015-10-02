## 固定効果とランダム効果モデル
library("plm")

## pooling dataとして分析
result.pooling <- plm(physint.imp.1 ~ lag.physint.imp.1 + competive.imp.1 + ele.sys3 + party1.imp.0 + jud.ind.imp.1
                      + pop.size.imp.1 + gdp.us.imp.1 + type2.imp.1 + type3.imp.1 
                      , data = qog.ts.2, effect = "individual", model = "pooling", index = c("cname", "year"))
summary(result.pooling)

## 固定効果モデル
result.within.plm <- plm(physint.imp.1 ~ lag.physint.imp.1 + competive.imp.1 + ele.sys3 + party1.imp.0 + jud.ind.imp.1
                         + pop.size.imp.1 + gdp.us.imp.1 + type2.imp.1 + type3.imp.1 
                         , data = qog.ts.2, effect = "individual", model = "within", index = c("cname", "year"))
summary(result.within.plm)
summary(fixef(result.within.plm))
## 変動効果モデル
result.random.plm <- plm(physint.imp.1 ~ lag.physint.imp.1 + competive.imp.1 + ele.sys3 + party1.imp.0 + jud.ind.imp.1
                         + pop.size.imp.1 + gdp.us.imp.1 + type2.imp.1 + type3.imp.1 
                         , data = qog.ts.2, effect = "individual", model = "random", index = c("cname", "year"))
summary(result.random.plm)

## モデル間比較(検定)
## F検定によるpoolingと固定効果モデルの比較
pFtest(result.within.plm, result.pooling)
## 固定効果モデルの方が良い

## poolingと変動効果モデルの比較
## Breusch-Pagan検定
plmtest(result.random.plm, "individual", "bp")

## 固定効果モデルと変動効果モデルの比較
## Hausman検定
phtest(result.within.plm, result.random.plm)

## lm関数を使ってpoolingデータとして分析
result.pooing.lm <- lm(physint.imp.1 ~ lag.physint.imp.1 + competive.imp.1 + ele.sys3 + party1.imp.0 + jud.ind.imp.1
                       + pop.size.imp.1 + gdp.us.imp.1 + type2.imp.1 + type3.imp.1 
                       , data = qog.ts.2)
summary(result.pooing.lm)
summary(result.pooling)

## 分析に使用したデータをcsvファイルとして保存
write.csv(qog.ts.2, file = "MData_fin.csv")
