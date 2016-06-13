library(EBImage)
img <- readImage("D:/1.png")
colorMode(img) = Grayscale; display(img, method = "raster", frame = 2)
img2 <- img[,,2] > 0.1; display(img2, method = "raster")
writeImage(img2, "D:/2.tiff", quality = 100)

kern = makeBrush(3, shape="disc")
img3 = dilate(erode(img2, kern), kern)
display(img3, method = "raster")
writeImage(img3, "D:/2.tiff", quality = 100)

system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'D:/2.tiff D:/1.txt -l eng -psm 1'), wait = TRUE)


##################
img <- readImage("D:/2.png");colorMode(img) = Grayscale;img <- img[,,2] > 0.1;display(img,method = "raster")

display(nick <- img[170:720,70:130], method = "raster")
writeImage(nick, "D:/nick.tiff", quality = 100)
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'D:/nick.tiff D:/nick.txt -l eng -psm 7'), wait = TRUE)

display(ap <- img[170:720,190:230], method = "raster")
writeImage(ap, "D:/ap.tiff", quality = 100)
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'D:/ap.tiff D:/ap.txt -l eng -psm 6'), wait = TRUE)

display(UPV <- img[500:720,1020:1060], method = "raster")
writeImage(UPV, "D:/UPV.tiff", quality = 100)
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'D:/UPV.tiff D:/UPV.txt -l eng -psm 7'), wait = TRUE)

doTesseract <- function(image){
  library(EBImage)
  img <- readImage(image); img <- channel(img, "gray"); img[img < 0.4] <- 0; img[img >= 0.4] <- 1
  kern = makeBrush(1, shape="disc")
  img = dilate(erode(img, kern), kern); display(img,method = "raster")
  
  display(temp <- img[170:720,70:130], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  name = strsplit(readLines("temp.txt"), " ")[[1]][1]; print(name)
  
  display(temp <- img[160:720,185:230], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  ap = as.numeric(gsub(",","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(ap)
  
  display(temp <- img[500:720,1020:1060], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  UPV = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(UPV)
  
  display(temp <- img[500:720,1060:1100], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  PD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(PD)
  
  display(temp <- img[450:720,1100:1150], method = "raster")
  writeImage(temp, "temp.tiff")
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 6'), wait = TRUE)
  # MAGIC :(
  temp <- readLines("temp.txt"); temp <- gsub("[[:punct:]]","", temp);temp <- gsub("XM","", temp) 
  XMC = as.numeric(gsub("[[:punct:]]","",strsplit(temp, " ")[[1]][1])); print(XMC)
  
  display(temp <- img[450:720,1200:1240], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 6'), wait = TRUE)
  DW = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(DW)
  
  display(temp <- img[450:720,1290:1330], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 6'), wait = TRUE)
  RD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(RD)
  
  display(temp <- img[450:720,1330:1375], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  LC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(LC)
  
  display(temp <- img[450:720,1375:1420], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  CFC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(CFC)
  
  display(temp <- img[450:720,1420:1460], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MUC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MUC)
  
  display(temp <- img[450:720,1460:1500], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  LLEC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(LLEC)
  
  display(temp <- img[450:720,1500:1540], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  LCF = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(LCF)
  
  display(temp <- img[450:720,1540:1580], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  XMR = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(XMR)
  
  display(temp <- img[450:720,1580:1620], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  PC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(PC)

  display(temp <- img[450:720,1620:1660], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  UPC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(UPC)

  display(temp <- img[450:720,1660:1700], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MD)

  display(temp <- img[450:720,1760:1800], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  RDes = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(RDes)
  
  display(temp <- img[450:720,1800:1840], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  PN = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(PN)

  display(temp <- img[450:720,1840:1880], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  ELD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(ELD)
  
  display(temp <- img[460:720,1880:1920], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  ECFD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(ECFD)

  display(temp <- img[450:720,1980:2020], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MTPH = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MTPH)

  display(temp <- img[450:720,2020:2060], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MTLM = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MTLM)

  display(temp <- img[450:720,2060:2100], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MLLD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MLLD)
  
  display(temp <- img[450:720,2100:2140], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  MTFH = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(MTFH)

  display(temp <- img[450:720,2140:2180], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  LFMD = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(LFMD)

  
  display(temp <- img[600:720,2240:2280], method = "raster")
  kern = makeBrush(3, shape="disc")
  temp = erode(dilate(temp, kern),kern); display(temp,method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 6'), wait = TRUE); readLines("temp.txt")
  UMC = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(UMC)
  
  display(temp <- img[450:720,2340:2380], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  H = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(H)
  
  display(temp <- img[450:720,2380:2420], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  GHP = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(GHP)
  
  display(temp <- img[450:720,2420:2455], method = "raster")
  writeImage(temp, "temp.tiff", quality = 100)
  system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'temp.tiff temp -l eng -psm 7'), wait = TRUE)
  LHS = as.numeric(gsub("[[:punct:]]","",strsplit(readLines("temp.txt"), " ")[[1]][1])); print(LHS)
  
  file.remove(c("temp.tiff", "temp.txt"))
  res <-data.frame(name,ap,UPV,PD,XMC,DW,RD,LC,CFC,MUC,LLEC,LCF,XMR,PC,UPC,MD,RDes,PN,ELD,ECFD,MTPH,MTLM,MLLD,MTFH,LFMD,UMC,H,GHP,LHS)
  return(res)

}

doTesseract("3.png")


library(EBImage)
img <- readImage("profiles/1.png"); colorMode(img) = Grayscale; img <- img[,,1]
display(img <- img[450:dim(img)[1],(dim(img)[2]-1450):dim(img)[2]],method = "raster")

img[img < 0.3] <- 0; img[img >= 0.3] <- 1; display(img,method = "raster")
kern = makeBrush(3, shape="disc"); img = dilate(erode(img, kern), kern); display(img,method = "raster")
img[img < 0.4] <- 0; img[img >= 0.4] <- 1
display(img,method = "raster")
### REMOVE NOISE WORDS
img[(dim(img)[1]-100):dim(img)[1],(dim(img)[2]-40):dim(img)[2]] <- 0
img[(dim(img)[1]-160):dim(img)[1],(dim(img)[2]-320):(dim(img)[2]-275)] <- 0
img[(dim(img)[1]-98):dim(img)[1],(dim(img)[2]-500):(dim(img)[2]-310)] <- 0
img[(dim(img)[1]-160):dim(img)[1],(dim(img)[2]-400):(dim(img)[2]-360)] <- 0
img[(dim(img)[1]-98):dim(img)[1],(dim(img)[2]-1040):(dim(img)[2]-1000)] <- 0
img[(dim(img)[1]-78):dim(img)[1],(dim(img)[2]-1000):(dim(img)[2]-960)] <- 0

display(img[(dim(img)[1]-78):dim(img)[1],(dim(img)[2]-1000):(dim(img)[2]-960)], method = "raster")
display(img, method = "raster")



writeImage(img, "101.tiff")
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'profiles/1.png 1 -l eng -psm 5'), wait = TRUE)






img <- readImage("profiles/8.png"); img <- channel(img, "gray"); display(img, method = "raster")
img[img < 0.1] <- 0; img[img >= 0.1] <- 1; display(img[(dim(img)[1]/1.5):dim(img)[1],1:dim(img)[2]],method = "raster")
#kern = makeBrush(1, shape="box"); img = dilate(erode(img, kern), kern); display(img,method = "raster")
# la <- matrix(1, nc=3, nr=3)
# la[2,2] <- -1
# img <- filter2(img, la); display(img,method = "raster")
display(img,method = "raster")
writeImage(img[(dim(img)[1]/2):dim(img)[1],1:dim(img)[2]], "1.png")
writeImage(img, "1.png")
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', '1.png 1 -l eng -psm 6'), wait = TRUE)





name <- img[175:500,80:120]
display(name,method = "raster")
h <- dim(img)[1]; v <- dim(img)[2]
for(x in 0:10) { display(img[1:h,(v-40*x-40):(v-40*x)],method = "raster") }




############ name
img <- readImage("profiles/1.png"); img <- channel(img, "gray"); display(img, method = "raster")
img[img < 0.1] <- 0; img[img >= 0.1] <- 1; display(img,method = "raster")
kern = makeBrush(3, shape="box"); img = dilate(erode(img, kern), kern); display(img,method = "raster")
la <- matrix(1, nc=3, nr=3)
la[2,2] <- -1
img <- filter2(img, la); display(img,method = "raster")
name <- img[175:500,80:120]; display(name,method = "raster")
name <- thresh(name, 10, 10, 0.05)
name <- opening(name, makeBrush(5, shape='disc'))
name <- bwlabel(name)
display(paintObjects(img, name, col='#ff00ff'), method = "raster")



#############################################################

img <- readImage("profiles/8.png"); img <- channel(img, "gray"); display(img, method = "raster")
img[img < 0.1] <- 0; img[img >= 0.1] <- 1; display(img[(dim(img)[1]/1.5):dim(img)[1],1:dim(img)[2]],method = "raster")
#kern = makeBrush(1, shape="box"); img = dilate(erode(img, kern), kern); display(img,method = "raster")
# la <- matrix(1, nc=3, nr=3)
# la[2,2] <- -1
# img <- filter2(img, la); display(img,method = "raster")
display(img,method = "raster")
writeImage(img[(dim(img)[1]/2):dim(img)[1],1:dim(img)[2]], "1.png")
writeImage(img, "1.png")
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', '1.png 1 -l eng -psm 6'), wait = TRUE)
system(paste('"C:/Program Files (x86)/Tesseract-OCR/tesseract.exe"', 'profiles/6.png res -l eng -psm 6'), wait = TRUE)
