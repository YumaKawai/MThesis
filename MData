## データセットの読み込み
qog.ts <- read.csv("qog_std_ts_jan15.csv")

## データの成型
# 1990年から2010年までのデータを使用
qog.ts <- subset(qog.ts, year > 1989)
qog.ts <- subset(qog.ts, year < 2011)
summary(qog.ts)

## ratify変数を結合
qog.ts <- merge(qog.ts, ratify, by.x= c("cname", "year"), by.y= c("CTRY", "YEAR"), all=F)

## csvファイルとして保存
write.csv(qog.ts, file = "qog.ts.csv")

## 使用する変数のみ抽出してデータセットを構築
qog.ts.1 <- data.frame("cmane" = qog.ts$cname, "year" = qog.ts$year, "ratify" = qog.ts$RATIFY, "physint" = qog.ts$ciri_physint,
                      "competive" = qog.ts$dpi_eipc, "party1" = qog.ts$gol_enep, "party2" = qog.ts$gol_enep1, 
                      "party3" = qog.ts$gol_enepo, "ele.sys1" = qog.ts$gol_est, "ele.system2" = qog.ts$gol_est_spec,
                      "type2" = qog.ts$ucdp_type2, "type3" = qog.ts$ucdp_type3, "pop.size1000" = qog.ts$gle_pop,
                      "gdp.us.per" = qog.ts$gle_rgdpc, "gdp.us" = qog.ts$gle_gdp, "gdp.imf" = qog.ts$imf_gdp, 
                      "rule.law" = qog.ts$fh_rol, "jud.ind" = qog.ts$bti_ij)
summary(qog.ts.1)

## 使用する変数の単位を変換
# GDP
qog.ts.1$gdp.us <- qog.ts.1$gdp.us * 100
qog.ts.1$gdp.us <- log(qog.ts.1$gdp.us)
hist(qog.ts.1$gdp.us)

# 人口
qog.ts.1$pop.size <- qog.ts.1$pop.size1000 * 1000
qog.ts.1$pop.size <- log(qog.ts.1$pop.size)
hist(qog.ts.1$pop.size)

## 紛争の変数をダミー変数化
qog.ts.1$type2 <- topcode(qog.ts.1$type2, 1)
qog.ts.1$type3 <- topcode(qog.ts.1$type3, 1)

## データをcsvファイルとして保存
write.csv(qog.ts.1, file = "MData.csv")
## 手作業にてラグ付き応答変数の投入
## データの読み込み
qog.ts.1 <- read.csv("MData.csv")
table(qog.ts.1$year)
summary(qog.ts.1)

## ICCPR加盟国のデータのみを抽出
qog.ts.2 <- subset(qog.ts.1, ratify == 1)
summary(qog.ts.2)

unique(qog.ts.2$cmane)
table(qog.ts.2$year)

## 2005年までのデータを使用(欠損値が多いため)
qog.ts.2 <- subset(qog.ts.2, year < 2006)
summary(qog.ts.2)
