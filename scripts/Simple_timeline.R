years.tl= c(1700, 1735,
            1795,1798,1809,1812,1830,1831,1836,
            1858,1859,1865,1869,1885,1892,1896,1900,
            1906,1915,1918,1942,1944,1948,1953,1965,1977,1990,1996,2003)
years.seq = seq(range(years.tl)[1],range(years.tl)[2], by=10)
years.seq2 = seq(range(years.tl)[1]+5,range(years.tl)[2]+5, by=10)
years.seq3 = seq(range(years.tl)[1]+1,range(years.tl)[2]+1, by=1)
years.tl = unique(years.tl)
plot(rep(0,length.out=length(years.tl))~years.tl,type="n",
     bty = 'n',yaxt='n',xlab = "Années", ylab = "")
# axis(side = 1, at = years.tl,#NA,
#      cex.axis=1,padj=-6, font=1, tck=.11)
axis(side = 1, at = years.seq,#NA,
     cex.axis=1,padj=-6, font=1, tck=.11)
axis(side = 1, at = years.seq2,#NA,
     cex.axis=0.5,padj=-10, font=1, tck=.05)
axis(side = 1, at = years.seq3,NA,
     cex.axis=0.5,padj=-8, font=1, tck=.025)
opar <- par(new = TRUE, ## add a new layer
            mar = c(0,0,0,0)) ## with no margins margins
y.arr = -1.08
  segments(1650,y.arr,2030,y.arr,
           col='black',
           lwd = 4)
  par(opar)
  