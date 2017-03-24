# testimg<-readPNG("IMG_1917.PNG")
# testimg<-readPNG("mandrill.png")
# testfname<-"mandrill"
testfname<-"IMG_1917"
testimg<-readPNG(paste0(testfname, ".png"))
pn<-2 #number of padding lines for raster arrangement
hn<-8 #inches of output image space in ggsave


df = data.frame(
  red = matrix(testimg[,,1], ncol=1),
  green = matrix(testimg[,,2], ncol=1),
  blue = matrix(testimg[,,3], ncol=1) #, 
  #  alpha = matrix(testimg[,,4], ncol=1)
)

lk=4
K = kmeans(df,lk)
df$label = K$cluster

### Replace the color of each pixel in the image with the mean 
### R,G, and B values of the cluster in which the pixel resides:

# get the coloring
colors = data.frame(
  label = 1:nrow(K$centers), 
  R = K$centers[,"red"],
  G = K$centers[,"green"],
  B = K$centers[,"blue"]  #, 
  #alp = K$centers[,"alpha"]
)

pdf(file = paste0(testfname, "_colorwheel_k", lk, ".pdf"))
pie(rep(1, lk), col=rgb(colors[,2:4]))
dev.off()

# merge color codes on to df
# IMPORTANT: we must maintain the original order of the df after the merge!
df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),]
df$order = NULL

# get mean color channel values for each row of the df.
R = matrix(df$R, nrow=dim(testimg)[1])
G = matrix(df$G, nrow=dim(testimg)[1])
B = matrix(df$B, nrow=dim(testimg)[1])
#Alp = matrix(df$alp, nrow=dim(testimg)[1])






dubcomb<-c("rrr", "ggg", "bbb", "rrg", "ggr", "bbr", "rrb", "ggb", "bbg", "rgg", "rbb", "grr", "gbb", "brr", "bgg", 
           "rgr", "rbr", "grg", "gbg", "brb", "bgb")
singcomb<-c("rgb", "grb", "bgr", "rbg", "gbr", "brg")
namecomb<-c(singcomb, dubcomb)

matcomb<-list(list(R, G, B), list(G, R, B), list(B, G, R), list(R, B, G), list(G, B, R), list(B, R, G), 
              list(R, R, R), list(G, G, G), list(B, B, B), list(R, R, G), list(G, G, R), list(B, B, R), list(R, R, B), list(G, G, B),
              list(B, B, G), list(R, G, G), list(R, B, B), list(G, R, R), list(G, B, B), list(B, R, R), list(B, G, G), 
              list(R, G, R), list(R, B, R), list(G, R, G), list(G, B, G), list(B, R, B), list(B, G, B))
names(matcomb)<-namecomb

# numcomb<-list(c(1,2,3), c(2,1,3), c(3,2,1), c(1,3,2), c(2,3,1), c(3,1,2), 
#               c(1,1,1), c(2,2,2), c(3,3,3), c(1,1,2), c(2,2,1), c(3,3,1), c(1,1,3), c(2,2,3), c(3,3,2), c(1,2,2), c(1,3,3), c(2,1,1),
#               c(2,3,3), c(3,1,1), c(3,2,2), c(1,2,1), c(1,3,1), c(2,1,2), c(2,3,2), c(3,1,3), c(3,2,3))
# names(numcomb)<-namecomb
imgsegl<-vector("list", length(matcomb))
names(imgsegl)<-namecomb
for (i in 1:length(matcomb)) {
  img.segmented = array(dim=c(dim(testimg)[1:2], 3))
  mm<-matcomb[[i]]
  img.segmented[,,1] = mm[[1]] #r
  img.segmented[,,2] = mm[[2]] #g
  img.segmented[,,3] = mm[[3]] #b
  
  imgp<-grobTree(rasterGrob(img.segmented, name=names(matcomb)[i]), textGrob(names(matcomb)[i], x=unit(0.5, "npc"), y=unit(0.05, "npc")))
  imgsegl[[i]]<-imgp
}
colcombo<-marrangeGrob(imgsegl, nrow=2, ncol=3, padding=unit(pn, "line"))
ggsave(paste0(testfname,"_k",lk,"_rgboptions.pdf"), colcombo, width=unit(7.12, "in"), height=unit(hn, "in"))



pdf(NULL) 
dev.control(displaylist="enable") 
pie(rep(1, lk), col=rgb(colors[,2:4]))
p1.base <- recordPlot() 
invisible(dev.off())
