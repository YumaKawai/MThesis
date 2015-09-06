## クラスター標準誤差の計算
## データの確認
summary(D_test$CTRY)
table(D_test$YYEAR)
length(unique(D_test$CTRY))

install.packages("gmm", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
library("gmm")
library("lmtest")

## クラスター標準誤差を計算する関数を定義(飯田2013 p75)
clr   <- function(dat,fm, cluster){  
  require(sandwich, quietly = TRUE)  
  require(lmtest, quietly = TRUE)  
  M <- length(unique(cluster))  
  N <- length(cluster)  
  K <- fm$rank  
  dfc <- (M/(M-1))*((N-1)/(N-K))  
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));  
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)  
  coeftest(fm, vcovCL) }  

clr(D_test, result.imp, D_test$CTRY)
summary(result.imp)

## 分析結果のLatex表示
require(stargazer)
stargazer(result.imp)


## キャタピラプロットの作成
df <- data.frame(variable = c("ラグ付き応答変数", "人口", "GDP",
                              "国内紛争レベル", "国際紛争レベル", "競争的選挙"),
                 mean = coef(result.imp)[2:7],
                 lower = c(a[1],b[1],c[1],d[1],e[1],f[1]),
                 upper = c(a[2],b[2],c[2],d[2],e[2],f[2]))
row.names(df) <- NULL

## キャタピラプロットを作る
## 95%信頼区間の計算
a <- c(0.293965 - 1.96*0.041366, 0.293965 + 1.96*0.041366)
b <- c(-0.098574 - 1.96*0.179015, -0.098574 + 1.96*0.179015)
c <- c(0.1037320 - 1.96*0.106342, 0.1037320 + 1.96*0.106342)
d <- c(-0.483417 - 1.96*0.157482, -0.483417 + 1.96*0.157482)
e <- c(0.028708 - 1.96*0.107344, 0.028708 + 1.96*0.107344)
f <- c(0.133366 - 1.96*0.046271, 0.133366 + 1.96*0.046271)

ctplr <- ggplot(df, aes(x = reorder(variable, lower),
                        y = mean,
                        ymin = lower,
                        ymax = upper)) +
  geom_pointrange(size=1.4) +
  geom_hline(aes(intercept = 0), linetype = "dotted") +
  xlab("説明変数") + ylab("係数の推定値") +
  coord_flip()
print(ctplr)
