#load image and convert to grayscale
testimg<-readPNG("IMG_1917.PNG")
yy<-rgb(testimg[,,1], testimg[,,2], testimg[,,3], alpha=testimg[,,4])
yg<-desaturate(yy)
dim(yy)<-dim(yg)<-dim(testimg)[1:2]
yn<-col2rgb(yg)[1,]/255
ynr<-apply(yn, 2, rev)
ynrt<-t(ynr)

#plots
op<-par(mar=c(0,0,4.1,0), mgp=c(2,.6,0), oma=c(.5,.5,4,.5), cex=.6, tcl=-.25)
par(mfrow=c(2,2))


image(1:748, 1:872, ynrt, col=grey.colors(128), axes=FALSE, asp=1, main="Original Image")

ynoise<-ynrt+matrix(rnorm(dim(ynrt)[1]*dim(ynrt)[2], sd=.2), dim(ynrt)[1], dim(ynrt)[2])
image(1:748, 1:872, ynoise, col=grey.colors(128), axes=FALSE, asp=1, main="Image with Noise")

ydnh<-denoise.dwt.2d(ynoise, wf="haar")
image(1:736, 1:864, ydnh, zlim=range(ydnh), col=grey.colors(128), axes=FALSE, asp=1, main="Denoised Image")

#image(1:748, 1:872, ynoise-denoise.dwt.2d(ynoise, wf="haar"), col=grey.colors(128), main="Residual Image")


img.R<-img.G<-img.B<-testimg
img.R[,,2:3]<-img.G[,,c(1,3)]<-img.B[,,1:2]<-0
img1 = rasterGrob(img.R)
img2 = rasterGrob(img.G)
img3 = rasterGrob(img.B)
grid.arrange(img1, img2, img3, nrow=1)

df = data.frame(
  red = matrix(testimg[,,1], ncol=1),
  green = matrix(testimg[,,2], ncol=1),
  blue = matrix(testimg[,,3], ncol=1) #, 
#  alpha = matrix(testimg[,,4], ncol=1)
)

lk=10
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

pie(rep(1, lk), col=rgb(colors[,2:4]))

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

# reconstitute the segmented image in the same shape as the input image
img.segmented = array(dim=c(dim(testimg)[1:2], 3))
img.segmented[,,1] = R
img.segmented[,,2] = G
img.segmented[,,3] = B
#img.segmented[,,4] = Alp

imgrgb<-rasterGrob(img.segmented)

grid.raster(img.segmented)

# png(filename="segmentedimg.png")
# grid.raster(img.segmented)
# dev.off()

#why is this in gray scale?
library("rgl")
# color space plot of mandrill
open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$red, df$green, df$blue),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE, axes=TRUE)
play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )

# color space plot of segmented mandrill
open3d()
plot3d(df$red, df$green, df$blue, 
       col=rgb(df$R, df$G, df$B),
       xlab="R", ylab="G", zlab="B",
       size=3, box=FALSE)
play3d( spin3d(axis=c(1,1,1), rpm=3), duration = 10 )


#playing with svd
img.svd<-svd(yn)
im.d<-diag(img.svd$d)
im.u<-img.svd$u
im.v<-img.svd$v
plot(1:length(img.svd$d), img.svd$d, xlim=c(0, 50))

depth <- 20
us <- as.matrix(im.u[, 1:depth])
vs <- as.matrix(im.v[, 1:depth])
ds <- as.matrix(im.d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
image(ls, asp=1, col=gray.colors(500))
