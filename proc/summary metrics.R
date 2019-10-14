##########################################
# USER SETTINGS

#define paths for input/output
#indir <- "/Users/mgoeck/MPI/science/conferences/1905 CH4 WS Fort Collins/data/Flux Data/half-hourly/"
#outdir <- "/Users/mgoeck/MPI/science/conferences/1905 CH4 WS Fort Collins/data/New variability metrics/" #note: folder is required for script to work!

options(stringsAsFactors = F)
indir <- "./data/"
outdir <- "./scripts/"

#select sites, define timeframe to analyze
file.IDs=c(0:60) #if first item is set to 0, use all files in list; otherwise, list site IDs to be processed (sites sorted alphabetically)
sel.years <- c(0) #if set to 0, use all timestamps from input data; otherwise, list years to be processed
month.list <- c(7:8) #if set to 0, use all months; otherwise, define months (as integers from 1-12) to be processed

#select input parameters
ch4.gapfill <- T   #set to T/F; if TRUE, use gapfilled column (FCH4_F), otherwise use only measured data (FCH4)

#select analysis components to be processed
wind.sector.check <- F  #set to T/F; if TRUE, analyze how many wind sectors cover the major parts of the dataset
median.cluster <- T  #set to T/F; if TRUE, analyze if there is a clustering of high/low wind sectors that indicates pronounced directional influences on fCH4
violin.stats <- T  #set to T/F; if TRUE, analyze distribution of fluxes along violin plot, condense to core metrics
stats.overview <- F  #set to T/F; if TRUE, key stats from the previous analyses will be compiled in a single overview table
cluster.variance <- F


#detailed settings for analysis components
#1.) wind.sector.check settings
WDcum.thresholds <- c(0.75, 0.9)  #define an arbitrary number of thresholds between 0 and 1. Routine will calculate how many wind sectors are needed to reach these thresholds
plot.WDcum <- T  #set to T/F; if TRUE, plot cumulative WD coverage in descending order
#2.) median.cluster settings
cluster.width <- 4  #widith of the moving window to determine sector-wise mean fluxes
plot.cluster <- T  #set to T/F; if TRUE, plot bar chart with normalized median flux per aggregated sector
#3.) violin plot stats
plot.violin.stats <- T  #set to T/F; if TRUE, plot bar chart depicting the normalized variability of medians multiplied by WD fractional coverage
#4.) additional plotting
plot.CH4.windrose <- T
add.violin.plot <- T
add.windrose.plot <- T


##########################################
# upload required packages
source(paste0(outdir, "windrose.R"))
if (!is.element("package:ggplot2", search())) library("ggplot2") 
if (!is.element("package:reshape", search())) library("reshape") 
if (!is.element("package:gridExtra", search())) library("gridExtra") 
if (!is.element("package:Hmisc", search())) library("Hmisc") 


####################################################################################
####################################################################################
# START PROCESSING

##########################################
# get site list
sites <- list.files(indir) #identify all files in input folder
if (file.IDs[1] != 0) {    #if required, reduce number of sites to be processed according to user-defined settings
  sites <- sites[file.IDs]
} else {
  file.IDs <- file.IDs[2:length(file.IDs)]
}

#clean up output arrays
if (exists("WDcum.table")) rm(WDcum.table)
if (exists("cluster.table")) rm(cluster.table)
if (exists("violin.table")) rm(violin.table)
if (exists("summary.table")) rm(summary.table)
if (exists("sites.proc")) rm(sites.proc)


# start to loop through sites
for (site.act in sites) {
  site.name <- substr(site.act, 1, 5)
  print(paste0("Processing site ", site.name))
  
  #create output folder for current site
  outdir.act <- paste0(outdir, site.name, "/")
  if (!file.exists(paste(outdir.act, "dummy.dat", sep=""))) {
    dir.create(outdir.act)
    file.create(paste(outdir.act, "dummy.dat", sep=""))
  }
  
  ##########################################
  # load dataset
  flux <- read.csv(paste0(indir, site.act), na.strings="NaN")
  if (is.element("TIMESTAMP_END", names(flux))) {
    flux.time <- strptime(flux[,"TIMESTAMP_END"],"%Y%m%d%H%M")
  } else {
    print("timestamp missing, skip site processing")
    next    
  }
  if (ch4.gapfill) {
    if (all(is.element(c("WD", "WS", "FCH4_F", "TA"), names(flux)))) {
      flux=flux[,c("WD", "WS", "TA", "FCH4_F")]
    } else {
      print("parameter missing, skip site processing")
      next
    }
    names(flux)[4]="FCH4"
  } else {
    if (all(is.element(c("WD", "WS", "FCH4", "TA"), names(flux)))) {
      flux=flux[,c("WD", "WS", "TA", "FCH4")]
    } else {
      print("parameter missing, skip site processing")
      next
    }    
  }
  if (exists("sites.proc")) {
    sites.proc <- c(sites.proc, site.name)
  } else {
    sites.proc <- site.name
  }
  
  #constrain timeframe according to user-defined settings
  year.flux=(as.integer(format(flux.time, format="%Y"))) #reduce dataset to user-defined years
  if (sel.years[1] != 0) {
    sel <- is.element(year.flux, sel.years)
    flux <- flux[sel,]
    flux.time <- flux.time[sel]; rm(year.flux, sel)
  }
  rm(year.flux)
  
  month.flux=(as.integer(format(flux.time, format="%m"))) #reduce dataset to user-defined months
  if (month.list[1] != 0) {
    sel <- is.element(month.flux, month.list)
    flux <- flux[sel,]
    flux.time <- flux.time[sel]; rm(sel)
  }
  rm(month.flux)
  
  #remove WD and FCH4 NAs
  sel <- !is.na(flux[,"WD"]) & !is.na(flux[,"FCH4"])
  flux <- flux[sel,]; rm(sel)
  
  #Add wind sector ID
  WD.mod <- as.numeric(flux[,"WD"]) + 15
  sel.WD <- !is.na(WD.mod) & WD.mod>=360
  WD.mod[sel.WD]=WD.mod[sel.WD]-360; rm(sel.WD)
  WD.class=ceiling(WD.mod/30)
  flux=cbind(flux, WD.class); rm(WD.class, WD.mod); gc()

  
  ##################################################
  ##################################################
  # pre-calculations: wind statistics and medians
  if (exists("WD.stats")) rm(WD.stats)
  for (WD.act in 1:12) {
    sel <- flux[,"WD.class"]==WD.act
    #calculate coverage fraction and median flux for wind sector
    int <- c((WD.act-1)*30, sum(sel)/dim(flux)[[1]], median(flux[sel,"FCH4"])) 
    if (WD.act == 1) {
      WD.stats=int
    } else {
      WD.stats=cbind(WD.stats, int)
    }
    rm(int, sel)
  }  #End of FOR (WD.act)
  rm(WD.act)
  # -> array 'WD.stats' contains coverage fraction (line 2) and median CH4 flux (line 3) for 12 wind sectors
  
  
  ##################################################
  ##################################################
  # analysis step 1: check for dominant wind sector
  if (wind.sector.check) {
    if (dim(flux)[[1]] != 0) {
      WD.cum <- cumsum(sort(WD.stats[2,], decreasing=T))  #sum up WD fractions in decending order of magnitude
      int <- max(WD.stats[2,])
      for (i in 1:length(WDcum.thresholds)) {
        int <- c(int, min(which(WD.cum>WDcum.thresholds[i])))
      }
      
      if (plot.WDcum) {
        png(paste0(outdir.act, site.name, "_WDcum.png"), width = 800, height = 768)
        par(cex=3)
        plot(WD.cum, type="l", col="red", lwd=5, xlab="# Sectors")
        graphics.off()  
      }
      
      out <- c(int, WD.stats[2,], WD.cum)
    } else {
      out <- rep(NA, dim(WDcum.table)[[1]])
    }
    
    if (exists("WDcum.table")) {
      WDcum.table <- cbind(WDcum.table, out)
    } else {
      WDcum.table <- out
    }
    rm(int, out, WD.cum); gc()
  } #End of IF (wind.sector.check)
  
  
  ##################################################
  ##################################################
  # analysis step 2: check for clustering of high/low flow sectors
  if (median.cluster) {
    if (dim(flux)[[1]] != 0) {
      med.norm <- (WD.stats[3,]-mean(WD.stats[3,]))/sd(WD.stats[3,])
      cluster.int <- c(med.norm, med.norm[1:(cluster.width-1)])
      for (i in 1:12) {
        if (i == 1) {
          cl.out <- mean(cluster.int[1:(cluster.width)])
        } else {
          cl.out <- c(cl.out, mean(cluster.int[i:(i+cluster.width-1)]))
        }
      }
      if (plot.cluster) {
        png(paste0(outdir.act, site.name, "_cluster-flux.png"), width = 800, height = 768)
        par(cex=3)
        plot(cl.out, type="h", col="red", lwd=10, xlab="wind sector start", ylab="norm. median fCH4")
        graphics.off()  
      }
      
      out <- c(max(cl.out)-min(cl.out), WD.stats[3,], cl.out)
      
      ##########################################
      # plot median methane windrose
      if (plot.CH4.windrose) {
        out <- t(WD.stats); out <- as.data.frame(out); names(out)=c("WD", "frequency", "CH4")
        wd.ch4 <- ggplot(out, aes(x = WD, y=frequency, fill = CH4)) + 
          geom_bar(width = 30, stat="identity") + 
          scale_x_continuous(breaks = 0:11/12*360, 
                             labels = c("N", "NNE", "ENE", "E", "ESE", "SSE", "S", "SSW", "WSW", "W", "WNW", "NNW"),  
                             name = "Wind Sectors") + 
          coord_polar(theta = "x", start=-pi/12) + 
          scale_fill_distiller(palette = "Spectral")
        ggsave(filename=paste0(outdir.act, site.name, "_CH4median-vs-WD_open.png"))
        graphics.off()
      }
    } else {
      out <- rep(NA, dim(cluster.table)[[1]])
    }
    
    if (exists("cluster.table")) {
      cluster.table <- cbind(cluster.table, out)
    } else {
      cluster.table <- out
    }
    rm(med.norm, cluster.int, out, cl.out); gc()
  }  #End of IF (median.cluster)
  
  
  ##################################################
  ##################################################
  # analysis step 3: check for clustering of high/low flow sectors
  if (violin.stats) {
    if (dim(flux)[[1]] != 0) {
      #use un-weighted means/sd
      #med.norm <- (WD.stats[3,]-mean(WD.stats[3,]))/sd(WD.stats[3,])  #normalize the median values of the 12 wind sectors by mean and stdev

      #calculate weighted median      
      stats.alt <- WD.stats[2:3, order(WD.stats[3,])]
      for (i in 2:11) {
        if (sum(stats.alt[1,1:(i-1)])<0.5 & sum(stats.alt[1,(i+1):12])<0.5) break
      }
      wtd.avg <- stats.alt[2,i]; rm(stats.alt) #use weighted median to shift fluxes
      #wtd.avg <- wtd.mean(WD.stats[3,], WD.stats[2,]) #alternative: used weighted mean to shift fluxes
      #calculate weighted standard deviation to normalize fluxes
      wtd.sd <- sqrt(sum(WD.stats[2,]*WD.stats[3,]^2)*sum(WD.stats[2,])-sum(WD.stats[2,]*WD.stats[3,])^2)/sum(WD.stats[2,])^2 
      med.norm <- (WD.stats[3,]-wtd.avg)/wtd.sd #normalize medians based on weighted parameters
      med.wgt <- med.norm^2*WD.stats[2,]  #multiply with wind sector fractional coverage as weight

      if (plot.violin.stats) {
        png(paste0(outdir.act, site.name, "_violin-stats_squared.png"), width = 1024, height = 768)
        par(cex=3)
        plot(med.wgt, type="h", col="red", lwd=10, xlab="wind sector", ylab="weighed median fCH4 deviation")
        graphics.off()  
      }  
      
      int <- c(sum(abs(med.wgt)), min(WD.stats[3,]), max(WD.stats[3,]), max(WD.stats[3,])-min(WD.stats[3,]), med.wgt)  
  
            
      ##################################################
      ##################################################
      # violin plots
      if (add.violin.plot) {
        
        if (exists("out2")) rm(out2)
        for (i in 1:dim(WD.stats)[[2]]) {
          int2 <- rep(WD.stats[3,i], WD.stats[2,i]*100000)
          if (i == 1) {
            out2=int2
          } else {
            out2=c(out2, int2)
          }
          rm(int2)
        }
        out2 <- as.data.frame(out2)
        names(out2) <- c("medianCH4")
        
        
        # make GGplot of windrose
        p <- ggplot(out2, aes(x="", y=medianCH4)) + 
          geom_violin(fill="red") +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
          geom_boxplot(width=0.1, fill="grey")
        
        ggsave(filename=paste0(outdir.act, site.name, "_CH4median-violin.png"))
        graphics.off()
        
        
        
        sort.ID <- sort(WD.stats[3,], index.return=T)$ix
        WD.sort <- WD.stats[,sort.ID]
        WD.sort <- rbind(WD.sort, cumsum(WD.sort[2,]))
        png(paste0(outdir.act, site.name, "_cumWD-fluxes.png"), width = 1024, height = 768)
        par(cex=2)
        plot(x=WD.sort[3,], y=WD.sort[4,], type="h", col="red", xlab="Median CH4 flux", ylab="cumulative frequency", lwd=3, ylim=c(0,1))
        par(new=T)
        plot(x=WD.sort[3,], y=WD.sort[4,], type="l", col="black", ylim=c(0,1), xlab=NA, ylab=NA)
        graphics.off()
        
        rm(out2, WD.sort)
        
      }  #End IF (add.violin.plot)
    } else {
        if (exists("violin.table")) { int <- rep(NA, dim(violin.table)[[1]]) }
    }
    
    if (exists("violin.table")) {  violin.table <- cbind(violin.table, int)
    } else { violin.table <- int }
    rm(int, med.norm, med.wgt)
  } #End of IF (violin.stats)
  
  ##################################################
  ##################################################
  # analysis step 4: minimize variance by clustering
  # if (cluster.variance ){
    # sd.aggr <- array(NA, dim=c(6,6))
    # mean.aggr <- array(NA, dim=c(6,6))
    # frac.aggr <- array(NA, dim=c(6,6))
    # 
    # sort.ID <- sort(WD.stats[3,], index.return=T)$ix
    # WD.sort <- WD.stats[,sort.ID]
    # WD.sort <- rbind(WD.sort, cumsum(WD.sort[2,])); rm(sort.ID)
    # 
    # #1 cluster
    # score.all <- sd(WD.sort[3,])
    # sd.aggr[1,1] <- sd(WD.sort[3,])
    # mean.aggr[1,1] <- sum(WD.sort[3,]*WD.sort[2,])
    # frac.aggr[1,1] <- sum(WD.sort[2,])
    # 
    # #2 cluster
    # sd.score=vector(mode="numeric", length=11)
    # for (i in 1:11) {
    #   sd.int <- c(sd(WD.sort[3,1:i]), sd(WD.sort[3,(i+1):dim(WD.sort)[[2]]]))
    #   sel=is.na(sd.int); sd.int[sel]=0
    #   sd.score[i]=sd.int[1]*i/12 + sd.int[2]*(12-i)/12
    # }
    # split <- which(rank(sd.score)==1); rm(sd.score, sd.int, sel)
    # sd.aggr[2,1:2] <- c(sd(WD.sort[3,1:split]), sd(WD.sort[3,(split+1):dim(WD.sort)[[2]]]))
    # mean.aggr[2,1:2] <- c(sum(WD.sort[3,1:split]*WD.sort[2,1:split])/sum(WD.sort[2,1:split]), sum(WD.sort[3,(split+1):dim(WD.sort)[[2]]]*WD.sort[2,(split+1):dim(WD.sort)[[2]]])/sum(WD.sort[2,(split+1):dim(WD.sort)[[2]]]))
    
    }  #End of IF (cluster.variance)
  
#}  #End of FOR (sites)

##################################################
##################################################
# save results
if (wind.sector.check) {
  dimnames(WDcum.table) <- list(c("WD_max", paste0(WDcum.thresholds*100, "%"), paste0("Sector ", c(0:11)*30), paste0("cumulative ", c(1:12))), sites.proc)
  write.table(t(WDcum.table), file=paste0(outdir, "WDtable.csv"), sep=",", col.names=T, row.names=T)
}

if (median.cluster) {
  dimnames(cluster.table) <- list(c("max.cluster diff.", paste0("fCH4-med_", c(0:11)*30), paste0("norm. flux, sector ", c(1:12))), sites.proc)
  write.table(t(cluster.table), file=paste0(outdir, "Cluster-table.csv"), sep=",", col.names=T, row.names=T)
}

if (violin.stats) {
  dimnames(violin.table) <- list(c("variability score", "min(median)", "max(median)", "range(median)", paste0("norm wgt median, sector ", c(1:12))), sites.proc)
  write.table(t(violin.table), file=paste0(outdir, "Violin-table.csv"), sep=",", col.names=T, row.names=T)
}

if (stats.overview) {
  if (wind.sector.check) {
    summary.table <- WDcum.table[1:(length(WDcum.thresholds)+1),]
    sum.names=c("WD_max", paste0(WDcum.thresholds*100, "%"))
  }
  if (median.cluster) {
    if (exists("summary.table")) {
      summary.table <- rbind(summary.table, cluster.table[1,])
      sum.names <- c(sum.names, "max.cluster diff.")
    } else {
      summary.table <- cluster.table[1,]
      sum.names <- "max.cluster diff."   
    }
  }
  if (violin.stats) {
    if (exists("summary.table")) {
      summary.table <- rbind(summary.table, violin.table[1:4,])
      sum.names <- c(sum.names, "variability score", "max(median)", "min(median)", "range(median)")
    } else {
      summary.table <- violin.table[1:4,]
      sum.names <- c("variability score", "min(median)", "max(median)", "range(median)")      
    }
  }
  dimnames(summary.table) <- list(sum.names, sites.proc)
  write.table(t(summary.table), file=paste0(outdir, "Summary-table.csv"), sep=",", col.names=T, row.names=T)
}
