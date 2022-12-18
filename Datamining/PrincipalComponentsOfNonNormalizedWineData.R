wine.df <- read.csv("Wine.csv")
head(wine.df)
pcs.cor <- prcomp(wine.df[,-1])#Wine Type = character이라서 제거
summary(pcs.cor)

pcs.cor$rotation#각 feature들의 weight 값

pcs.cor$rot[,1:4]
