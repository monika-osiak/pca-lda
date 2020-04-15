spine <- read.csv("dataset-spine.csv")

spine.pr <- prcomp(spine[c(1:12)], 
                   center=TRUE, 
                   scale=TRUE)


pc1_var <- as.double(summary(spine.pr)$importance[,1][2])
pc2_var <- as.double(summary(spine.pr)$importance[,2][2])
print(sprintf('Wariancja danych zawarta w pierwszym składniku wiodącym: %s%%', format(round(pc1_var, 2), nsmall = 2)))
print(sprintf('Wariancja danych zawarta w drugim składniku wiodącym: %s%%', format(round(pc2_var, 2), nsmall = 2)))


screeplot(spine.pr, 
          type="l", 
          npcs=15, 
          main="Screeplot of the first 10 PCs")
abline(h=1, 
       col="red", 
       lty=5)
legend("topright", 
       legend=c("Eigenvalue = 1"),
       col=c("red"), 
       lty=5, 
       cex=0.6)

cumpro <- cumsum(spine.pr$sdev^2 / sum(spine.pr$sdev^2))
plot(cumpro[0:45], 
     xlab="PC #",
     ylab="Amount of explained variance", 
     main="Cumulative variance plot")
legend("topleft", 
       legend=c("Cut-off @ PC6"),
       col=c("blue"), 
       lty=5, 
       cex=0.6)

x_label <- sprintf('składnik wiodący 1: (%s%%)', format(round(pc1_var, 2), nsmall = 2))
y_label <- sprintf('składnik wiodący 2: (%s%%)', format(round(pc2_var, 2), nsmall = 2))
plot_title <- 'Dane po redukcji rozmiaru do dwóch wymiarów'
plot(spine.pr$x[,1],
     spine.pr$x[,2], 
     xlab=x_label, 
     ylab=y_label, 
     main=plot_title)
grid(nx=NULL,
     ny=NULL, 
     col="lightgray", 
     lty="dotted")


library("factoextra")
fviz_pca_ind(spine.pr, 
             geom.ind="point", 
             pointshape=21, 
             pointsize=2, 
             fill.ind=spine$class, 
             col.ind="black", 
             palette="jco", 
             addEllipses=TRUE,
             label="var",
             col.var="black",
             repel=TRUE,
             legend.title="Class") +
               ggtitle("Dane z zaznaczonymi klasami") +
               theme(plot.title=element_text(hjust=0.5))