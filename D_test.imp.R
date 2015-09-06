## データの読み込み
## D_test.csv

## 欠損値の処理
summary(D_test)

## See Gelman and Hill (2007): p.534
topcode <- function(a, top){
  return(ifelse(a>top, top, a))
}

## 単純無作為補完
## Gelman and Hill (2007): p.534
random.imp <- function(a){
  missing <- is.na(a)        ## 欠測かどうか確かめる
  n.missing <- sum(missing)  ## 欠測値の数
  a.obs <- a[!missing]       ## 観測値の数
  imputed <- a
  ## 観測された値をランダムに選んで欠測値を補完する
  imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
  return(imputed)
}

## 観察されたliecの分布
require(ggplot2)
theme_set(theme_gray(base_size = 12, base_family = "HiraKakuProN-W3"))

liec.obs <- ggplot(D_test, aes(liec)) + geom_histogram(binwidth=1)
liec.obs + labs(x="LIEC", title="観測値")

## 欠損値を補完
D_test$LIEC.imp.1 <- random.imp(D_test$liec)
D_test$LIEC.imp.1 <- topcode(D_test$LIEC.imp.1, 7)

## 補完されたデータの分布
liec.imp.1 <- ggplot(D_test, aes(LIEC.imp.1)) + geom_histogram(binwidth=1)
liec.imp.1 + labs(x="LIEC", title="無作為に補完された値")

## 分析で使用するデータについて全て補完する
## 応答変数PHYSINT
D_test$PHYSINT.imp.1 <- random.imp(D_test$PHYSINT)
D_test$PHYSINT.imp.1 <- topcode(D_test$PHYSINT.imp.1, 8)

## 統制変数
D_test$lag.PHYSINT.imp.1 <- random.imp(D_test$lag.PHYSINT)
D_test$lag.PHYSINT.imp.1 <- topcode(D_test$lag.PHYSINT.imp.1, 8)

max(D_test$interConf, na.rm = TRUE)
D_test$interConf.imp.1 <- random.imp(D_test$interConf)
D_test$interConf.imp.1 <- topcode(D_test$interConf.imp.1, 3)

max(D_test$NATLOG_NEW_POPSZ, na.rm = TRUE)
D_test$NATLOG_NEW_POPSZ.imp.1 <- random.imp(D_test$NATLOG_NEW_POPSZ)
D_test$NATLOG_NEW_POPSZ.imp.1 <- topcode(D_test$NATLOG_NEW_POPSZ.imp.1, 20)

max(D_test$NATLOG_NEW_GDPCAP, na.rm = TRUE)
D_test$NATLOG_NEW_GDPCAP.imp.1 <- random.imp(D_test$NATLOG_NEW_GDPCAP)
D_test$NATLOG_NEW_GDPCAP.imp.1 <- topcode(D_test$NATLOG_NEW_GDPCAP.imp.1, 10)

D_test$domesticConf.imp.1 <- random.imp(D_test$domesticConf)
D_test$domesticConf.imp.1 <- topcode(D_test$domesticConf.imp.1, 3)

## 補完したデータを使って分析
result.imp <- lm(PHYSINT.imp.1 ~ lag.PHYSINT.imp.1 + NATLOG_NEW_POPSZ.imp.1 + NATLOG_NEW_GDPCAP.imp.1
                 + domesticConf.imp.1 + interConf.imp.1 + LIEC.imp.1 + as.factor(YYEAR) + as.factor(CTRY)
                 , data = D_test)
summary(result.imp)

## result.impが最新の分析結果
## D_testをcsvとして保存
write.csv(D_test, file = "D_test.csv")

## キャタピラプロットの作成
df <- data.frame(variable = c("ラグ付き応答変数", "人口", "GDP",
                              "国内紛争レベル", "国際紛争レベル", "競争的選挙"),
                 mean = coef(result.imp)[2:7],
                 lower = confint(result.imp)[2:7, 1],
                 upper = confint(result.imp)[2:7, 2])
row.names(df) <- NULL

## キャタピラプロットを作る
ctplr <- ggplot(df, aes(x = reorder(variable, lower),
                        y = mean,
                        ymin = lower,
                        ymax = upper)) +
  geom_pointrange(size=1.4) +
  geom_hline(aes(intercept = 0), linetype = "dotted") +
  xlab("説明変数") + ylab("係数の推定値") +
  coord_flip()
print(ctplr)

