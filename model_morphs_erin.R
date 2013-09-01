wd <- "~/morphs-analysis/"
setwd(wd)

library(stats)
require(logspline)

#for discretization:
grid.steps = 64
grid = seq(0,1,length.out=grid.steps)
cache.index = function(v) {
  return(1+round(v*(grid.steps-1)))
}

#example items from experiment, 15 each distribution
down.examples <- c(0.04583, 0.003231, 0.07391, 0.01884, 0.00003024, 0.04158,
                   0.09081, 0.06746, 0.01949, 0.1007, 0.1633, 0.1441, 0.1655,
                   0.2697, 0.2161)
mid.examples <- c(0.31404, 0.30456, 0.39520, 0.56064, 0.49728, 0.53187, 0.55993,
                  0.47519, 0.54332, 0.48362, 0.51678, 0.44763, 0.68272, 0.61375,
                  0.69832)
unif.examples <- c(0.9730805, 0.0589135, 0.1332413, 0.5568001, 0.6201130, 0.4243146,
                   0.4176713, 0.2215742, 0.6778150, 0.6834636, 0.8716204, 0.5641932,
                   0.3503760, 0.9606276, 0.0048311)
examples <- list(down.examples, mid.examples, unif.examples)
names(examples) <- c("down", "mid", "unif")
possible.utterances = c('no-utt', 'pos', 'very pos') #probably OK since Ss see all of
#these in same page
utterance.lengths = c(0,1,5)
utterance.polarities = c(0,+1,+1)


# kernel.granularity <- grid.steps #2^12 #how many points are calculated for the kernel density estimate
# est.kernel <- function(dist, bw, adjust) {
#   return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
#                  kernel="gaussian", bw=bw, adjust=adjust))
# }

#norms the kernel density
#using logspline to fit density (respects boundaries)
make.pdf.cache <- function(dist) {
  k <- dlogspline(grid, logspline(dist, lbound=0,ubound=1))
  k <- k/sum(k) #normalize
  k <- k+0.0001 #regularize away from 0 (in prob units)
  k <- k/sum(k) #then normalize again
  return(k)
}

#creates fn that approximates percentage of area before x
#takes in all the points where kernel density is estimated
make.cdf.cache <- function(dist) {
  cumulants <- cumsum(make.pdf.cache(dist))
  return(cumulants)
}


##caching. (R has strange purity on global assignments, so must use <<- to set cache)
L0.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
S1.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
cache.misses=0 #to track whether caching is working right.

clear.cache = function(){
  print(paste("L0 cache NAs:", length(is.na(L0.cache))))
  cache.misses<<-0
  L0.cache <<- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
  S1.cache <<- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
}


listener0 = function(utterance.idx, thetas.idx, degree.idx, pdf, cdf, thetaGtr) {
  
  if(is.na(L0.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    theta.order = !(thetaGtr & (grid[thetas.idx[1]]>=grid[thetas.idx[2]]))
    if (utterance.idx == 1) { #assume the null utterance
      L0.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx] <<- theta.order* pdf[degree.idx]
    }  else if(utterance.polarities[utterance.idx] == +1) {
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] >= grid[theta.idx]  
      true.norm = if(theta.idx==1){1} else {1-cdf[theta.idx-1]}
      L0.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx] <<- theta.order* utt.true * pdf[degree.idx] / true.norm
    } else {
      theta.idx = thetas.idx[utterance.idx-1]
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] <= grid[theta.idx] 
      true.norm = cdf[theta.idx]
      L0.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx] <<- theta.order* utt.true * pdf[degree.idx] / true.norm
    }
  }
  return(L0.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx])
}

speaker1 = function(thetas.idx, degree.idx, utterance.idx, alpha, utt.cost, pdf, cdf, thetaGtr) {
  
  if(is.na(S1.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    utt.probs = array(0,dim=c(length(possible.utterances)))
    for(i in 1:length(possible.utterances)) {
      l0 = listener0(i, thetas.idx, degree.idx, pdf, cdf, thetaGtr)
      utt.probs[i] <- (l0^alpha) * exp(-alpha * utt.cost *  utterance.lengths[i])
    }
    S1.cache[degree.idx,thetas.idx[1],thetas.idx[2],] <<- utt.probs/sum(utt.probs)
  }
  
  return(S1.cache[degree.idx,thetas.idx[1],thetas.idx[2],utterance.idx])
}

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size,
                     dist, band.width, thetaGtr, adjust) {
  
  utt.idx = which(possible.utterances == utterance)
  
  #kernel.est <- est.kernel(dist, band.width, adjust)
  pdf <- make.pdf.cache(examples[[dist]])
  cdf <- make.cdf.cache(examples[[dist]])
  
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
  samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
  
  
  #scoring function, to compute (unormalized) probability of state. (should be in log domain?)
  prob.unnormed = function(state) {
    #check bounds:
    if (any(state < 0) || any(state > 1)) {return(0)}
    degree.idx = cache.index(state[1])
    thetas.idx = c(cache.index(state[2]), cache.index(state[3]))#sapply(thetas,cache.index)
    #prior for degree (thetas have unif prior):
    prior = pdf[degree.idx]
    #probbaility speaker would have said this (given state):
    likelihood = speaker1(thetas.idx, degree.idx, utt.idx, alpha, utt.cost, pdf, cdf, thetaGtr)
    return(prior*likelihood)
  }
  
  #initialize chain by rejection:
  print("initializing")
  state.prob=0
  state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
  while(state.prob==0) {
    state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
    state.prob = prob.unnormed(state)
  }
  samples[1,] = state
  
  #make an MH proposal, spherical gaussian on degree and thetas. 
  make.proposal = function(v) {
    perturbations = rnorm(length(v), mean = 0, sd = step.size)
    return(v + perturbations)
  }
  
  #run mcmc chain:
  print("running mcmc")
  n.proposals.accepted = 0
  for (i in 2:n.samples) {
    proposal = make.proposal(state)
    proposal.prob = prob.unnormed(proposal)
    #MH acceptance, assumes proposal is symmetric:
    if(runif(1,0,1) <= min(1, proposal.prob/state.prob)) {
      n.proposals.accepted = n.proposals.accepted + 1
      state = proposal
      state.prob = proposal.prob
    }
    samples[i,] = state
  }
  
  print("acceptance rate:")
  print(n.proposals.accepted/(n.samples-1))
  print("misses since last cache clear:")
  print(cache.misses)
  
  return(list(samples=samples, prop.accepted=n.proposals.accepted/(n.samples-1)))
}

dists <- c("down", "mid", "unif")
myapply <- function(f) {
  c(sapply(dists, function(dist) {
    return(c(sapply(possible.utterances, function(utterance) {
      return(f(dist, utterance))
    })))
  }))
}


convert.logit <- F

logit <- function(v) {
  if (convert.logit) {
  return(sapply(v, function(p) {
    return(log(p) - log(1-p))
  }))
  } else {
  return(v)
  }
}

logistic <- function(v) {
  if (convert.logit) {
  return(sapply(v, function(x) {
    return(1/(1+exp(-x)))
  }))
  } else {
  return(v)
  }
}

#horribly messy graph function
kernel.dens.plot <- function(model.runs, label, logitify) {
  if (logitify) {
    convert.logit <<- T
  } else {
    convert.logit <<- F
  }
  n.samples <- length(model.runs[["down"]][["no-utt"]][["samples"]][,"degree"])
  distributions <- myapply(function(d,u) {
    return(rep(d, n.samples))
  })
  modifiers <- myapply(function(d,u) {
    return(rep(u, n.samples))
  })
  mp <- myapply(function(d,u) {
    df <- model.runs[[d]][[u]]
    return(df[["samples"]][,"degree"])
  })
  mydata <- data.frame(dist=distributions, mod=modifiers, mp=mp)
  png(paste(c(label, "logit", logitify, "-kernel-dens-est.png"), collapse=""), 2200, 1500, pointsize=32)
  par(mfrow=c(3,4))
  lapply(dists, function(d) {
    #f <- density(logit(examples[[d]]), kernel="gaussian", bw="sj")
    f$x <- seq(0,1,length.out=512)
    f$y <- dlogspline(f$x, logspline(examples[[d]], lbound=0,ubound=1))
    #     if (d == "unif") {
    #       xlab <- "feppiness"
    #       ylab <- "density"
    #     } else {
    xlab=""
    ylab=""
    #     }
    plot(logistic(f$x), f$y, type="l", main="", xlab=xlab, ylab=ylab, xlim=c(0,1), #ylim=c(0,length(f$x)),
         font.main=32, lwd=3)
    lapply(possible.utterances, function(m) {
      #       if (d == "unif" && m == "no-utt") {
      #         xlab <- "feppiness"
      #         ylab <- "density"
      #       } else {
      xlab=""
      ylab=""
      #       }
      samples <- mydata$mp[mydata$dist == d & mydata$mod == m]
      #f <- density(logit(samples), kernel="gaussian", bw="sj")
      f$x <- seq(0,1,length.out=512)
      f$y <- dlogspline(f$x, logspline(samples, lbound=0,ubound=1))
      plot(logistic(f$x), f$y, type="l", main="", ylab=ylab, xlab=xlab, xlim=c(0,1), #ylim=c(0,length(f$x)),
           font.main=32, lwd=3)
      mu <- mean(samples)
      abline(v = mu, col="blue", lwd=7)
    })
  })
  dev.off()
}

#run model with these values of parameters
model <- function(alpha, utt.cost, thetaGtr, label, adjust) {
  n.true.samples <- 3000#30000 #number of samples to keep
  lag <- 5 #number of samples to skip over
  burn.in <- 10
  n.samples <- n.true.samples * lag + burn.in
  step.size <- 0.03 #note this may not be appropriate for all conditions.
  
  model.runs <- lapply(dists, function(dist) {
    clear.cache()
    return(lapply(possible.utterances, function(utterance) {
      listener1(utterance, alpha=alpha, utt.cost=utt.cost, n.samples=n.samples,
                step.size=step.size, dist=dist, band.width="SJ", thetaGtr=thetaGtr, adjust)
    }))
  })
  model.runs <- lapply(model.runs, function(run) {
    names(run) <- possible.utterances
    return(run)
  })
  names(model.runs) <- dists
  #kernel.dens.plot(model.runs, label, logitify=T)
  kernel.dens.plot(model.runs, label, logitify=F)
  
  graph.dist <- myapply(function(d,u){return(d)})
  graph.utterance <- myapply(function(d,u){return(u)})
  graph.means <- myapply(function(d,u){
    du.name <- paste(c(label, "-", d, "-", u, ".data"), collapse="")
    du.frame <- model.runs[[d]][[u]]
    #save all data
    write.table(du.frame, du.name)
    return(mean(du.frame[["samples"]][,"degree"]))
  })
  write.table(graph.means, paste(c(label, "-means.data"), collapse=""))
  graph.data <- (matrix(data=graph.means, nrow=3, ncol=3,
                        dimnames=list(c("none", "adj", "very"),
                                      c("peakedDown", "peakedMid", "uniform"))))
  png(paste(c(label, ".png"), collapse=""), 1200, 800, pointsize=32)
  graph.title <- "Novel Adjective Model"
  novel.adj.bar <- barplot(as.matrix(graph.data), main=graph.title,
                           ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
  legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
  dev.off()
  return(graph.means)
}

timestamp <- as.character(unclass(Sys.time()))

mainDir <- wd
subDir <- paste(c("output", timestamp), collapse="")

if (!(file.exists(subDir))) {
  dir.create(file.path(mainDir, subDir))
}

time.label <- function(alpha, cost, very.len, adjust, i) {
  return(paste(c("output", timestamp, "/alpha", alpha, "_cost", cost, "_very.len", very.len,
                 "_adjust", adjust, "_run", i), collapse=""))
}

expt.means <- c(0.3774988, 0.2063296, 0.4692256, 0.6403353, 0.5309518, 0.8261740, 0.6875057, 0.5141071, 0.9364525)
#run the model with different values of free parameters
sapply(1:1, function(i) {
  sapply(c(5), function(alpha) {
    sapply(c(1), function(cost) {
      sapply(c(2), function(adjust) {
        model.means <- model(alpha=alpha, utt.cost=cost, thetaGtr=F,
                             label=time.label(alpha, cost, 2, adjust, i), adjust)
        print(paste("Correlation:", cor(model.means, expt.means)))
        print(paste("adjust:", adjust))
        print(paste("alpha:", alpha))
      })
    })
  })
})
