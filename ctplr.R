## キャタピラプロットの作成
## 95%信頼区間の計算
## 下限
lower <- rep(NA, 182)
for(i in 1:182){
  lower[i] <- b[i] - (SE[i] * 1.96)
}

## 上限
upper <- rep(NA, 182)
for(i in 1:182){
  upper[i] <- b[i] + (SE[i] * 1.96)
}

## ggplot用のデータフレームの作成
mi.df <- data.frame(variable = c("ラグ付き応答変数", "有効政党数", "人口規模", "経済規模", "裁判所の独立性",
                              "国際紛争ダミー", "内戦ダミー"),
                 mean = b[2:8],
                 lower = lower[2:8],
                 upper = upper[2:8])
row.names(mi.df) <- NULL

## キャタピラプロットの作成
require(ggplot2)
theme_set(theme_gray(base_size = 12, base_family = "HiraKakuProN-W3"))
ctplr <- ggplot(mi.df, aes(x = reorder(variable, lower),
                        y = mean,
                        ymin = lower,
                        ymax = upper)) +
  geom_pointrange(size=1.4) +
  geom_hline(aes(intercept = 0), linetype = "dotted") +
  xlab("説明変数") + ylab("係数の推定値") +
  coord_flip()
print(ctplr)

## 見にくいので改善
## ggplot用のデータフレームの作成
mi.df.2 <- data.frame(variable = c("ラグ付き応答変数", "有効政党数", "人口規模", "経済規模",
                                 "裁判所の独立性"),
                    mean = b[2:6],
                    lower = lower[2:6],
                    upper = upper[2:6])
row.names(mi.df.2) <- NULL

## キャタピラプロットの作成
require(ggplot2)
theme_set(theme_gray(base_size = 12, base_family = "HiraKakuProN-W3"))
ctplr.2 <- ggplot(mi.df.2, aes(x = reorder(variable, lower),
                           y = mean,
                           ymin = lower,
                           ymax = upper)) +
  geom_pointrange(size=1.4) +
  geom_hline(aes(intercept = 0), linetype = "dotted") +
  xlab("説明変数") + ylab("係数の推定値") +
  coord_flip()
print(ctplr.2)
