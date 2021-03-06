doTesseract <- function(image) {
  library("EBImage")
  img <- readImage(image)
  FRACTION = ifelse(mean(imageData(img[175:200,80:130,3])) > mean(imageData(img[175:200,80:130,2])), "RES", "ENL")
  
  img <- channel(img, "gray");# display(img, method = "raster")
  nick <- img[175:600,80:130];# display(nick, method = "raster")
  writeImage(nick, "temp.png")
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.png res -l eng -psm 6'), wait = TRUE)
  name <- strsplit(readLines("res.txt")," ")[[1]][1]
  
  AP <- img[175:600,190:230];# display(nick, method = "raster")
  writeImage(AP, "temp.png")
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.png res -l eng -psm 6'), wait = TRUE)
  AP <- as.numeric(gsub("[[:punct:]]", "", strsplit(readLines("res.txt"),"AP")[[1]][1]))
  
  writeImage(img, "temp.png")
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.png res -l eng -psm 6'), wait = TRUE)
  
  res <- readLines("res.txt")
  LVL <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("LVL", res)]))
  # Discovery
  UPV <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Unique Portals Visited", res)]))
  PD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Portals Discovered", res)]))
  XMC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("XM Collected", res)]))
  # Health
  DW <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Distance Walked", res)]))
  # Building
  RDep <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Resonators Deployed", res)]))
  LC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Links Created", res)]))
  CFC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Control Fields Created", res)]))
  MUC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Mind Units Captured", res)]))
  LLEC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Longest Link Ever Created", res)]))
  LCF <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Largest Control Field", res)]))
  XMR <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("XM Recharged", res)]))
  PC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Portals Captured", res)]))
  UPC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Unique Portals Captured", res)]))
  MD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Mods Deployed", res)]))
  # Combat
  RDes <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Resonators Destroyed", res)]))
  PN <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Portals Neutralized", res)]))
  ELD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Enemy Links Destroyed", res)]))
  ECFD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Enemy Control Fields Destroyed", res)]))
  # Defense
  MTPH <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Max Time Portal Held", res)]))
  MTLM <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Max Time Link Maintained", res)]))
  MLLD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Max Link Length x Days", res)]))
  MTFH <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Max Time Field Held", res)]))
  LFMD <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Largest Field MUs x Days", res)]))
  # Missions
  UMC <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Unique Missions Completed", res)]))
  MDA <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Mission Day", res)]))
  # Resource Gathering
  H <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Hacks", res)]))
  GHP <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Glyph Hack Points", res)]))
  LHS <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Longest Hacking Streak", res)]))
  # Mentoring
  ASR <- as.numeric(gsub("[^0-9]",replacement = "", res[grepl("Agents Successfully Recruited", res)]))
  
  
  l <- list(name,FRACTION,AP,LVL,UPV,PD,XMC,DW,RDep,LC,CFC,MUC,LLEC,LCF,XMR,PC,UPC,MD,RDes,PN,ELD,ECFD,MTPH,MTLM,MLLD,MTFH,LFMD,UMC,MDA,H,GHP,LHS,ASR)
  l <- ifelse(is.na(sapply(l,"[",1)),0,sapply(l,"[",1))
  df <- data.frame(rbind(l), stringsAsFactors=FALSE); df[,3:33] <- as.numeric(df[,3:33])
  names(df) <- c("name","FRACTION","AP","LVL","UPV","PD","XMC","DW","RDep","LC","CFC","MUC","LLEC","LCF","XMR","PC","UPC","MD","RDes","PN","ELD","ECFD","MTPH",
                 "MTLM","MLLD","MTFH","LFMD","UMC","MDA","H","GHP","LHS","ASR")
  file.remove(c("res.txt","temp.png"))
  return(df)
}

doTesseract("test.profiles/8.png")

df <- do.call(rbind, lapply(dir("test.profiles/", full.names = T), doTesseract))

d.st <- df[,3:33]/df$AP; row.names(d.st) <- df$name

clusters <- hclust(dist(d.st))
plot(clusters, labels = df$name)

library("ade4"); library("factoextra")
res.pca <- dudi.pca(d.st, scannf = FALSE); s.corcircle(res.pca$co, sub = "", possub = "topleft")

fviz_pca_biplot(res.pca, geom = "text") + theme_minimal()
fviz_pca_biplot(res.pca)

fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()


FRACTION <- as.factor(df$FRACTION)
fviz_pca_ind(res.pca, habillage = FRACTION, addEllipses = TRUE, ellipse.level = 0.68) + theme_minimal()
