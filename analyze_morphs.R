#install.packages("rjson")
#install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")
library(rjson)
library(stats)
#library(plyr)
#library(R.basic)

#get rid of first and last quotes. for some reason there are a bunch in my data :/ oops!
cutQuotes <- function(quoteyString) {
  startLetter <- 2
  stopLetter <- nchar(quoteyString) - 1
  return(substr(quoteyString, startLetter, stopLetter))
}

################# reading and cleaning data for all pieces
setwd("~/Code/cocolab/analyzing_experiments/morphs-analysis/")  ###change this to actual location of repo
rd <- read.table("morphs.results", sep="\t", quote='"', header=TRUE)

adjectives <- lapply(as.character(rd$Answer.adjective), cutQuotes)
nounlists <- lapply(as.character(rd$Answer.nouns), fromJSON)
distlists <- lapply(as.character(rd$Answer.distNames), fromJSON)
subjects <- rd$workerid
nsubj <- length(subjects)
subj.levels <- levels(subjects)

#useful for getting info from embedded json
getVal <- function(tag) {
  return( function(elem) {
    val <- elem[[tag]]
    return(val)
  })
}

# function from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x, upper, x, lower, angle=90, code=3, length=length, ...)
}


################# get subjects' responses to target questions
getPhrase <- function(dist, distlist, modifier, adj, nounlist) {
  noun <- nounlist[which(distlist == dist)]
  if (modifier == "very") {
    mod <- paste("very", adj, "")
  } else if (modifier == "adj") {
    mod <- paste(adj,"")
  } else if (modifier == "none") {
    mod <- ""
  } else {
    print("ERROR 2")
  }
  phrase <- paste( c(mod, noun), collapse="")
  return(phrase)
}

#parse json strings
targets <- lapply(as.character(rd$Answer.target), fromJSON)
ntargets <- length(targets)

# #create levels for the columns of the target data frame
# dist.levels <- c("down", "mid", "unif")
mod.levels <- c("none", "adj", "very")

distributions <- c(sapply(1:ntargets, function(i){
  df <- targets[[i]]
  nounlist <- nounlists[[i]]
  distlist <- distlists[[i]]
  adj <- adjectives[[i]]
  dists <- c(sapply(distlist, function(x){return(rep(x,3))}))
  return(dists)
}))

other.distributions <- c(sapply(1:ntargets, function(i){
  df <- targets[[i]] #target data for this subj
  nounlist <- nounlists[[i]] #noun list for this subj
  distlist <- distlists[[i]] #dist list for this subj
  adj <- adjectives[[i]] #adjective for this subj
  dists <- c(sapply(rev(distlist), function(x){return(rep(x,3))}))
  return(dists)
}))

subj.col <- c(sapply(1:ntargets, function(i){
  subj <- subjects[[i]]
  return(rep(subj, 6))
}))

reactions <- c(sapply(1:ntargets, function(i){
  rt <- targets[[i]]$rt
  return(rep(rt, 6))
}))

responses <- c(sapply(1:ntargets, function(i){
  df <- targets[[i]]
  nounlist <- nounlists[[i]]
  distlist <- distlists[[i]]
  adj <- adjectives[[i]]
  resps <- c(sapply(distlist, function(dist) {
    dist.responses <- sapply(mod.levels, function(mod){
      phrase <- getPhrase(dist, distlist, mod, adj, nounlist)
      resp <- df[[phrase]]
      return(resp[[length(resp)]])
    })
    return(dist.responses)
  }))
  return(resps)
}))

modifiers <- c(rep(mod.levels, ntargets*2))

#create target data frame
target.data <- data.frame(subj=subj.col, #worker id
                          dist=distributions, #where the distribution is peaked (down, mid, or unif)
                          mp=responses, #degree modifier (very, adj, none)
                          mod=modifiers, #morph proportion of chosen picture
                          rt=reactions, #reaction time for ENTIRE target section
                          other.dist=other.distributions) #other distribution this subj saw

#################### get subjects' responses to warmup questions
warmups <- lapply( as.character(rd$Answer.warmups), fromJSON)

nwarm <- 10
# # # if (nwarm != length(warmups)) {print("error1")}
# # # 
# # # order <- c(sapply(1:nwarm, function(i){
# # #   warmup <- warmups[[i]]
# # #   order <- sapply(1:length(warmup), function(j) {
# # #     question <- warmup[[j]]
# # #     return(question$order)
# # #   })
# # #   return(order)
# # # }))
# 
# 
nrow <- nsubj*nwarm

# #create columns of the warmup data frame
subj.col <- factor(nrow)
type.col <- factor(nrow)
#correctness.col <- factor(nrow)
order.col <- factor(nrow)
response.col <- factor(nrow)
levels(subj.col) <- subj.levels
levels(type.col) <- c("compare", "classify")
#levels(correctness.col) <- c("correct", "incorrect", NA)
levels(order.col) <- c("01", "10", "LM", "ML")
levels(response.col) <- c("left", "right", "down", "mid", "unif")

#create target data frame
warmup.data <- data.frame(subj=subj.col, #worker id
                          qnum=integer(nrow), #question number ONE-BASED!!!!
                          type=type.col,
                          rt=numeric(nrow),
                          correctness=numeric(nrow),
                          order=order.col, #either (0,1,2) in some order where 0
                          #is the peaked down category and 2 is the peaked up
                          #category or (L,M) in some order where L is the less
                          #"feppy" (or whatever adj) and M is the more feppy.
                          #Order is from left to right and it represents the
                          #order in which the buttons were displayed on screen
                          class.mp=numeric(nrow), #morp proportion for shape in
                                                  #classify-type warmup question
                          left.mp=numeric(nrow), #morph proportion for left shape
                                                 #in compare-type warmup question
                          right.mp=numeric(nrow), #morph proportion for right shape
                                                  #in compare-type warmup question
                          response=response.col,
                          lowerdist=character(nrow),
                          higherdist=character(nrow)
)

index <- 1
for (s in 1:nsubj) {
  subj <- as.character(subjects[[s]])
  nounlist <- nounlists[[s]]
  distlist <- distlists[[s]]
  warmup <- warmups[[s]]
  if (nwarm != length(warmup)) {print("ERROR 3")}
  for (q in 1:length(warmup)) {
    question <- warmup[[q]]
    if (q - 1 != question$qNumber) {print("ERROR 4")}
    type <- question$qType
    rt <- question$rt
    raw.response <- question$response
    if (raw.response == "left" || raw.response == "right") {
      response <- raw.response
    } else {
      response <- distlist[which(nounlist == raw.response)]
    }
    correctness <- question$correctness
    if (correctness == "N/A") {
      corr <- NA
    } else if (correctness == "correct") {
      corr <- 1
    } else if (correctness == "incorrect") {
      corr <- 0
    } else {print("ERROR 6")}
    mp <- question[["morphProps"]]
    if (length(mp) == 1) {
      class.mp <- as.numeric(mp)
      left.mp <- NA
      right.mp <- NA
    } else if (length(mp) == 2) {
      class.mp <- NA
      left.mp <- mp[[1]]
      right.mp <- mp[[2]]
    } else { print("ERROR 5") }
    order <- paste(as.character(question$order), collapse="")
    lower <- distlist[[1]]
    higher <- distlist[[2]]
    warmup.data[index,] <- c(subj, q, type, rt, corr, order, class.mp,
                             left.mp, right.mp, response, lower, higher)
    index <- index + 1
  }
}
warmup.data$correctness <- as.numeric(warmup.data$correctness)

# write.table(warmup.data, "warmup.data")
# 
################ subject exclusion

exclusion.list <- c()

# based on correctness of compare warmups
avg.correct <-aggregate(x=as.numeric(warmup.data$correctness)[warmup.data$type == "compare"],
                        by=list(warmup.data$subj[warmup.data$type == "compare"]), FUN="mean")
exclusion.list <- c(exclusion.list, as.character(avg.correct$Group.1[avg.correct$x < 0.5]))

# # how well they did on the obvious ones
# obvious.compare <- c(0.15, 0.55, 0.45, 0.85)
# obvious.class <- c(0.52, 0.92)
# obv.compare.ind <- warmup.data$type=="compare" & (is.element(warmup.data$left.mp, obvious.compare))
# obv.class.ind <- warmup.data$type=="classify" & (sapply(1:length(warmup.data$class.mp), function(i) {
#   return(is.element(warmup.data$class.mp[[i]], obvious.class))
# }))
# class.correct <- sapply(1:nrow(warmup.data), function(i) {
#   class.mp <- as.numeric(warmup.data$class.mp)[[i]]
#   if (is.na(class.mp) || !(is.element(class.mp, obvious.class))) {
#     return(NA)
#   } else {
#     response <- warmup.data$response[[i]]
#     higher <- warmup.data$higherdist
#     lower <- warmup.data$lowerdist
#     if (class.mp == 0.52) {
#       correct.class <- "down"
#     } else if (class.mp == 0.92) {
#       correct.class <- higher
#     } else {
#       print("error7")
#     }
#     if (response == correct.class) {
#       return(1)
#     } else {
#       return(0)
#     }
#   }
# })
# warmup.data$class.correct <- class.correct
# avg.obvious.compare.correct <- aggregate(x=as.numeric(warmup.data$correctness)[obv.compare.ind],
#                                          by=list(warmup.data$subj[obv.compare.ind]), FUN="mean")
# avg.obvious.class.correct <- aggregate(x=as.numeric(warmup.data$class.correct)[obv.class.ind],
#                                        by=list(warmup.data$subj[obv.class.ind]), FUN="mean")

# based on reaction time

#good data
exclusion.list <- unique(exclusion.list)
good.subjects <- subjects[!(is.element(subjects, exclusion.list))]

good.data <- target.data[(is.element(target.data$subj, good.subjects)),]
good.data$mp <- as.numeric(good.data$mp)
good.data$rt <- as.numeric(good.data$rt)

# ################ z scores
z.mp <- c(sapply(good.subjects, function(s) {
    subj.data <- subset(good.data, good.data$subj==s)
    return(scale(subj.data$mp))
}))
z.subj <- c(sapply(good.subjects, function(s) {
  return(rep(s, 6))
}))
z.dist <- c(sapply(good.subjects, function(s) {
  subj.data <- subset(good.data, good.data$subj==s)
  return(subj.data$dist)
}))
z.mod <- c(sapply(good.subjects, function(s) {
  subj.data <- subset(good.data, good.data$subj==s)
  return(subj.data$mod)
}))
z.other.dist <- c(sapply(good.subjects, function(s) {
  subj.data <- subset(good.data, good.data$subj==s)
  return(subj.data$other.dist)
}))
z.data <- data.frame(subj=z.subj, dist=z.dist, mod=z.mod, mp=z.mp, other.dist=z.other.dist)
# 
################ anovas
raw.lm <- lm(mp ~ dist*mod, data=good.data)
raw.anova <- anova(raw.lm)#aov(mp ~ dist*mod, data=good.data)
print(raw.anova)

othercat.lm <- lm(mp ~ dist*mod*other.dist, data=good.data)
othercat.anova <- anova(othercat.lm)
print(othercat.anova)

z.lm <- lm(mp ~ dist*mod, data=z.data)
z.anova <- anova(z.lm)#aov(mp ~ dist*mod, data=good.data)
print("z-scored:")
print(z.anova)

zothercat.lm <- lm(mp ~ dist*mod*other.dist, data=z.data)
zothercat.anova <- anova(zothercat.lm)
print(zothercat.anova)

dists <- c("down", "mid", "unif")
mods <- c("none", "adj", "very")
################ graph
mygraph <- function(mydata, range, mytitle) {
  avg.data <- aggregate(mp ~ dist + mod, data = mydata, FUN = mean)
  
  mp <- unlist(lapply(dists, function(d) {
    lapply(mods, function(m) {
      avg.data$mp[avg.data$dist == d & avg.data$mod == m]
    })
  }))
  
  graph.data <- (matrix(data=unlist(mp), nrow=3, ncol=3,
                       dimnames=list(c("none", "adj", "very"),
                                     c("peakedDown", "peakedMid", "uniform"))))
  novel.adj.bar <- barplot(as.matrix(graph.data), main=mytitle,
                           ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=range)
  legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
  
  ### confidence intervals
  conf.ints <- lapply(dists, function(dist) {
    return(lapply(mods, function(mod) {
      sub.subjs <- mydata$subj[mydata$mod == mod & mydata$dist == dist]
      sub.mps <- mydata$mp[mydata$mod == mod & mydata$dist == dist]
      nsubjs <- length(unique(sub.subjs))
      sample.means <- replicate(100, mean(sample(sub.mps, nsubjs, replace=TRUE)))
      return(quantile(sample.means, c(0.025, 0.975)))
    }))
  })
  conf.low <- c(sapply(conf.ints, function(conf.list) {
    return(c(sapply(conf.list, function(conf.pair) {return(conf.pair[1])})))
  }))
  conf.high <- c(sapply(conf.ints, function(conf.list) {
    return(c(sapply(conf.list, function(conf.pair) {return(conf.pair[2])})))
  }))
  lower <- (matrix(data=conf.low, nrow=3, ncol=3,
                        dimnames=list(c("none", "adj", "very"),
                                      c("peakedDown", "peakedMid", "uniform"))))
  higher <- (matrix(data=conf.high, nrow=3, ncol=3,
                        dimnames=list(c("none", "adj", "very"),
                                      c("peakedDown", "peakedMid", "uniform"))))
  error.bar(novel.adj.bar, as.matrix(graph.data), higher, lower)
}

mygraph(good.data, c(0,1), "Novel Adj Scale")
mygraph(z.data, c(-1.5,1.5), "Novel Adj Scale (z-scored)")



par(mfrow=c(3,3))
lapply(dists, function(d) {
  lapply(mods, function(m) {
    samples <- good.data$mp[good.data$dist == d & good.data$mod == m]
    f <- density(samples, kernel="gaussian", bw="nrd", from=0, to=1)
    plot(f$x, f$y, type="l", main=paste(d, m))
    abline(v = mean(samples), col="blue")
  })
})