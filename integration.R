## 係数の統合
## 分析結果から係数を抽出
b1 <- coef(result.1)
b2 <- coef(result.2)
b3 <- coef(result.3)

b <- b1 + b2 + b3
b <- b / 3

## 標準誤差の統合
## 標準誤差の計算
se1 <- clr(IMP.1, result.1, IMP.1$cname)
se2 <- clr(IMP.2, result.2, IMP.2$cname)
se3 <- clr(IMP.3, result.3, IMP.3$cname)

## 結果を保存するベクトルの作成
W <- rep(NA, 182)
W2 <- rep(NA, 182)
B <- rep(NA, 182)
B2 <- rep(NA, 182)
V <- rep(NA, 182)
SE <- rep(NA, 182)

## withinの計算
for(i in 1:182){
  W[i] <- (se1[i,2])^2 + (se2[i,2])^2 + (se3[i,2])^2
  W2[i] <- W[i] / 3
}

## betweenの計算
for(i in 1:182){
  B[i] <- (b1[i] - b[i]) + (b2[i] - b[i]) + (b3[i] - b[i])
  B2[i] <- B[i] / 2
}

## 統合
for(i in 1:182){
  V[i] <- W2[i] + ((4 / 3) * B2[i])
}

for(i in 1:182){
  SE[i] <- sqrt(V[i])
}

## データフレームに保存
df <- data.frame("coef" = b, "SE" = SE)
