#install.packages("rjson")
#install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")
library(rjson)
library(stats)
library(plyr)
library(R.basic)

#get rid of first and last quotes. for some reason there are a bunch in my data :/ oops!
cutQuotes <- function(quoteyString) {
  startLetter <- 2
  stopLetter <- nchar(quoteyString) - 1
  return(substr(quoteyString, startLetter, stopLetter))
}

################# reading and cleaning data for all pieces
setwd("~/Code/cocolab/running_experiments/2013-8-27_morphs_exp")
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

subjects <- c(sapply(1:ntargets, function(i){
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
target.data <- data.frame(subj=subjects, #worker id
                          dist=distributions, #where the distribution is peaked (down, mid, or unif)
                          mp=responses, #degree modifier (very, adj, none)
                          mod=modifiers, #morph proportion of chosen picture
                          rt=reactions) #reaction time for ENTIRE target section

# #################### get metadata on subjects
# # what shapes they saw
# # what color each shape was
# # what order they saw objects in
# # what names the objects were given
# # whether and how much they played with the adjective slider in the beginning
# # etc.
# 
# sliderPractice <- lapply(as.character(rd[["Answer.sliderPractice"]]), fromJSON)
# most <- lapply(as.character(rd$Answer.most), fromJSON)
# least <- lapply(as.character(rd$Answer.least), fromJSON)
# comments <- unlist( lapply( as.character(rd$Answer.comments), cutQuotes))
# language <- unlist( lapply(tolower(as.character(rd$Answer.language)), cutQuotes))
# 
# meta.data <- data.frame(subj = subjects,
#                         windowWidth = as.integer(rd$Answer.windowWidth),
#                         sliderPracticeMin = as.numeric(lapply(sliderPractice, getVal("min"))),
#                         sliderPracticeMax = as.numeric(lapply(sliderPractice, getVal("max"))),
#                         language = as.factor(language),
#                         leastLabel = as.factor( unlist(lapply(least, getVal("label")))),
#                         mostLabel = as.factor( unlist(lapply(most, getVal("label")))),
#                         age = as.integer(lapply(as.character(rd$Answer.age), cutQuotes)),
#                         # because I logged the data in the wrong direction, the noun whose
#                         # distribution is peaked up, which I call "mostNoun" is actually indexed in
#                         # the results as "least", and vice versa.
#                         mostNoun = as.factor( unlist( lapply(nounlists, getVal("least")))),
#                         leastNoun = as.factor( unlist( lapply(nounlists, getVal("most")))),
#                         midNoun = as.factor( unlist( lapply(nounlists, getVal("mid")))),
#                         adjective = as.factor(unlist(adjectives)),
#                         comments = as.factor(unlist(comments)),
#                         #order nouns are displayed from top to bottom where 0 is least and 2 is most
#                         warmupTableDisplayOrder = as.factor( rd$Answer.warmupIndices)
#                         )
# 
# write.table(meta.data, "meta.data")
# 
# 
# 
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
                          response=response.col
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
      class.mp <- mp
      left.mp <- NA
      right.mp <- NA
    } else if (length(mp) == 2) {
      class.mp <- NA
      left.mp <- mp[[1]]
      right.mp <- mp[[2]]
    } else { print("ERROR 5") }
    order <- paste(as.character(question$order), collapse="")
    warmup.data[index,] <- c(subj, q, type, rt, corr, order, class.mp, left.mp, right.mp, response)
    index <- index + 1
  }
}
warmup.data$correctness <- as.numeric(warmup.data$correctness)

# write.table(warmup.data, "warmup.data")
# 
################ subject exclusion

exclusion.list <- c()

# # based on comments
# confused.comment <- "I+was+confused+on+the+last+slide+but+I+did+my+best+to+try+and+understand+what+the+slider+was+meant+to+judge.+"
# unsure.comment <- "think+i+reversed+spiff+and+gub+not+sure."
# exclusion.list <- c(exclusion.list, as.character(subjects[meta.data$comment == confused.comment]))
# exclusion.list <- c(exclusion.list, as.character(subjects[meta.data$comment == unsure.comment]))

# based on warmups
avg.correct <-aggregate(x=as.numeric(warmup.data$correctness)[warmup.data$type == "compare"],
                        by=list(warmup.data$subj[warmup.data$type == "compare"]), FUN="mean")
exclusion.list <- c(exclusion.list, as.character(avg.correct$Group.1[avg.correct$x < 0.5]))

# based on reaction time

#good data
exclusion.list <- unique(exclusion.list)
good.subjects <- subjects[!(is.element(subjects, exclusion.list))]

good.data <- target.data[(is.element(target.data$subj, good.subjects)),]
good.data$mp <- as.numeric(good.data$mp)
good.data$rt <- as.numeric(good.data$rt)

# ################ z scores
# z <- c()
# for (s in subjects) {
#   subj.data <- subset(good.data, good.data$subj==s)
#   subj.z <- scale(subj.data$mp)
#   z <- c(z, subj.z)
# }
z.data <- data.frame(subj=good.data$subj, dist=good.data$dist, mod=good.data$mod, mp=z)
# #z.means <- aggregate(z ~ dist + mod, data = z.data, FUN = mean)
# 
################ anova
adj.anova <- aov(mp ~ dist*mod, data=good.data)
print(summary(adj.anova))
# 
################ graph
mygraph <- function(mydata, range, mytitle) {
  avg.data <- aggregate(mp ~ dist + mod, data = mydata, FUN = mean)
  
  down.none <- avg.data$mp[avg.data$dist == "down" & avg.data$mod == "none"]
  down.adj <- avg.data$mp[avg.data$dist == "down" & avg.data$mod == "adj"]
  down.very <- avg.data$mp[avg.data$dist == "down" & avg.data$mod == "very"]
  mid.none <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "none"]
  mid.adj <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "adj"]
  mid.very <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "very"]
  unif.none <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "none"]
  unif.adj <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "adj"]
  unif.very <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "very"]
  
  mp <- c(down.none, down.adj, down.very,
          mid.none, mid.adj, mid.very,
          unif.none, unif.adj, unif.very)
  
  graph.data <- (matrix(data=mp, nrow=3, ncol=3,
                       dimnames=list(c("none", "adj", "very"),
                                     c("peakedDown", "peakedMid", "uniform"))))
  novel.adj.bar <- barplot(as.matrix(graph.data), main=mytitle,
                           ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=range)
  legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
  
  ### confidence intervals
  dists <- c("down", "mid", "unif")
  mods <- c("none", "adj", "very")
  print(summary(mydata))
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
# mygraph(z.data, c(-2,2), "z-scored novel adj scale")
# 
# lapply(good.subjects, function(s) {
#   png(paste(c("subjects/", as.character(s)), collapse=""), 600, 400)
#   a <- good.data$mp[good.data$subj == s]
#   gd <- matrix(a, nrow=3, ncol=3,
#                dimnames=list(c("none", "adj", "very"),
#                              c("peakedDown", "peakedMid", "peakedUp")))
#   g <- barplot(gd, main=as.character(s),
#                ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
#   legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
#   dev.off()
# })
# 
# #################model output
# model.data <- lapply(0:9, function(i) {
#   filename <- paste(c('model-data/data_gtr', i), collapse='')
#   return(read.table(filename))
# })
# 
# mean.model <- matrix(rep(0,9), nrow=3, ncol=3,
#                dimnames=list(c("none", "adj", "very"),
#                              c("peakedDown", "peakedMid", "peakedUp")))
# lower.model <- matrix(rep(0,9), nrow=3, ncol=3,
#                dimnames=list(c("none", "adj", "very"),
#                              c("peakedDown", "peakedMid", "peakedUp")))
# higher.model <- matrix(rep(0,9), nrow=3, ncol=3,
#                 dimnames=list(c("none", "adj", "very"),
#                               c("peakedDown", "peakedMid", "peakedUp")))
# for (row in 1:3) {
#   for (col in 1:3) {
#     model.runs <- rep(0,10)
#     for (s in 1:10) {
#       model.runs[s] <- model.data[[s]][row,col]
#     }
#     mean.model[row,col] <- mean(model.runs)
#     sample.means <- replicate(100, mean(sample(model.runs, 10, replace=TRUE)))
#     conf <- quantile(sample.means, c(0.025, 0.975))
#     lower.model[row,col] <- conf[[1]]
#     higher.model[row,col] <- conf[[2]]
#   }
# }
# 
# png("model", 600, 400)
# model.bar <- barplot((mean.model), main="Novel Adj Model (very > adj)",
#                          ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
# legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
# 
# ### confidence intervals
# error.bar(model.bar, as.matrix(mean.model), higher.model, lower.model)
# dev.off()
# 
# getTime <- function(elem) {
#   timey.wimey <- strsplit(as.character(elem), " ")[[1]][4]
#   minutes <- as.numeric(strsplit(timey.wimey, ":")[[1]][2])
# }
# accept <- sapply(rd$assignmentaccepttime, getTime)
# submit <- sapply(rd$assignmentsubmittime, getTime)
# print(mean(submit - accept))
