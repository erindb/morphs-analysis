###change this to actual location of repo
wd <- "~/morphs-analysis/"
setwd(wd)

library(stats)

#for speaker1 discretization:
grid.steps = 100
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

#using r function density to find kernal density, so it's not actually continuous
kernel.granularity <- grid.steps#2^12 #how many points are calculated for the kernel density estimate
est.kernel <- function(dist, bw) {
  return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
                 kernel="gaussian", bw=bw, adjust=1))
}

#norms the kernel density
#takes in all the points where kernel density is estimated
make.pdf <- function(kernel.est) {
  area <- sum(kernel.est$y) 
  normed.dens <- kernel.est$y/area
  return(function(x){
    if (x < 0 || x > 1) {
      return(0)
    } else {
      return(normed.dens[cache.index(x)])
    }
  })
}

#creates fn that approximates percentage of area before x
#takes in all the points where kernel density is estimated
make.cdf <- function(kernel.est) {
  area <- sum(kernel.est$y) #total area covered by kernel
                                                 #density estimate
  cumulants <- cumsum(kernel.est$y/area)
  
  return(function(x){
    if (x <= 0) {
      return(0)
    } else if (x >= 1) {
      return(1)
    } else {
      return(cumulants[cache.index(x)])
    }
  })
}

#erin did not change this fn
polarity = function(utterance) {
	if (utterance == 'pos' || utterance == 'very pos' || utterance == 'pos1' ||
      utterance == 'very pos1' || utterance == 'pos2' || utterance == 'pos3') {
		return('positive')
	} else if (utterance == 'neg' || utterance == 'very neg' || utterance == 'neg1' ||
             utterance == 'very neg1' || utterance == 'neg2' || utterance == 'neg3') {
		return('negative')
	} else {
		print("Error in function polarity: unknown utterance")
	}
}

L0.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
S1.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))

clear.cache = function(){
  L0.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
  S1.cache <- array(NA,dim = c(grid.steps,grid.steps,grid.steps,length(possible.utterances)))
}

#todo: put back erin's Gtr test..
listener0 = function(utterance, t1, t2, d, pdf, cdf, thetaGtr) {
  ut = which(possible.utterances == utterance)
  
  if(is.na(L0.cache[d,t1,t2,ut])) {
    theta = if(ut==2){grid[t1]} else {grid[t2]}
    deg = grid[d]
    if (utterance == 'no-utt') {
      L0.cache[d,t1,t2,ut] <- pdf(deg)
    }	else if(polarity(utterance) == 'positive') {
      L0.cache[d,t1,t2,ut] <- (deg >= theta) * pdf(deg) / 1-cdf(theta)
    } else {
      L0.cache[d,t1,t2,ut] <- (deg <= theta) * pdf(deg) / cdf(theta)
    }
  }
  return(L0.cache[d,t1,t2,ut])
}

#erin did not change this fn
mylnth = function(u) {
	if (u == 'no-utt') {
		return(0)
	} else if (u=='very pos' || u=='very neg') {
		return(2)
	} else {
		return(1)
	}
}

speaker1 = function(thetas, degree, utterance, alpha, utt.cost, pdf, cdf, thetaGtr) {
  #round to grid. 
  d = cache.index(degree)
  t1 = cache.index(thetas[1])
  t2 = cache.index(thetas[2])
  ut = which(possible.utterances == utterance)
  
  eval.utt = function(utt) {
		l0 = listener0(utt, t1, t2, d, pdf, cdf, thetaGtr)
		return( (l0^alpha) * exp(-alpha * utt.cost *  mylnth(utt)))
	}
  
	if(is.na(S1.cache[d,t1,t2,ut])) {
    utt.probs = sapply(possible.utterances, FUN=eval.utt)
    S1.cache[d,t1,t2,] <- utt.probs/sum(utt.probs)
	}
  
	return(S1.cache[d,t1,t2,ut])
}

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size,
                     dist, band.width, thetaGtr) {
  
  kernel.est <- est.kernel(dist, band.width)
  pdf <- make.pdf(kernel.est)
  cdf <- make.cdf(kernel.est)
    
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
	samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
  
    
  #scoring function, to compute (unormalized) probability of state. (should be in log domain?)
  prob.unnormed = function(state) {
    #check bounds:
    if (any(state < 0) || any(state > 1)) {return(0)}
    degree = state[1]
    thetas = state[2:length(state)]
    #prior for degree (thetas have unif prior):
    prior = pdf(degree)
    #probbaility speaker would have said this (given state):
    likelihood = speaker1(thetas, degree, utterance, alpha, utt.cost, pdf, cdf, thetaGtr)
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

#run model with these values of parameters
model <- function(alpha, utt.cost, thetaGtr, label) {
  n.true.samples <- 1000#30000 #number of samples to keep
  lag <- 5#50 #number of samples to skip over
  burn.in <- 10#5000
  n.samples <- n.true.samples * lag + burn.in
  step.size <- 0.05
  
  model.runs <- lapply(dists, function(dist) {
    clear.cache()
    return(lapply(possible.utterances, function(utterance) {
      listener1(utterance, alpha=alpha, utt.cost=utt.cost, n.samples=n.samples,
                step.size=step.size, dist=dist, band.width="SJ", thetaGtr=thetaGtr)
    }))
  })
  model.runs <- lapply(model.runs, function(run) {
    names(run) <- possible.utterances
    return(run)
  })
  names(model.runs) <- dists
  
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
  png(paste(c(label, ".png"), collapse=""))
  graph.title <- paste(c("model alpha=", alpha, ", cost=", utt.cost), collapse="")
  novel.adj.bar <- barplot(as.matrix(graph.data), main=graph.title,
                           ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
  legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
  dev.off()
}

timestamp <- as.character(unclass(Sys.time()))

mainDir <- wd
subDir <- paste(c("output", timestamp), collapse="")

if (!(file.exists(subDir))) {
  dir.create(file.path(mainDir, subDir))
}

time.label <- function(identifier, i) {
  return(paste(c("output", timestamp, "/", identifier, "-run", i), collapse=""))
}

#run the model with different values of free parameters
nruns <- 1
sapply(1:nruns, function(i) {
  model(alpha=1, utt.cost=1, thetaGtr=F, label=time.label("alpha1cost2", i))
})

# #graph pdf and cdf
# sapply(dists, function(d) {
#   kernel.est <- est.kernel(d, bw="SJ")
#   dens <- make.pdf(kernel.est)
#   int <- make.cdf(kernel.est)
#   x.vals <- seq(-1, 2, 0.01)
#   pdf.y <- sapply(x.vals, dens)
#   cdf.y <- sapply(x.vals, int)
#   plot(x.vals, pdf.y, type="l", main=d, ylab="", xlab="", ylim=c(0,1))
#   par(new=T)
#   plot(x.vals, cdf.y, type="l", main=d, ylab="", xlab="", ylim=c(0,1))
# })
