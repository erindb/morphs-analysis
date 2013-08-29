setwd("~/morphs-analysis/")  ###change this to actual location of repo

library(stats)

#for speaker1 discretization:
grid.steps = 10

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
kernel.granularity <- 2^12 #how many points are calculated for the kernel density estimate
est.kernel <- function(dist, bw) {
  return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
                 kernel="gaussian", bw=bw, adjust=1))
}

#norms the kernel density
#takes in all the points where kernel density is estimated
make.pdf <- function(kernel.est) {
  area <- sum(kernel.est$y*1/kernel.granularity)
  return(function(x){
    if (x < 0 || x > 1) {
      return(0)
    } else {
      closest.x.index <- which.min(abs(x - kernel.est$x))
      un.normed <- kernel.est$y[closest.x.index]
      return(un.normed/area)
    }
  })
}

#creates fn that approximates percentage of area before x
#takes in all the points where kernel density is estimated
make.cdf <- function(kernel.est) {
  area <- sum(kernel.est$y*1/kernel.granularity) #total area covered by kernel
                                                 #density estimate
  return(function(x){
    if (x <= 0) {
      return(0)
    } else if (x >= 1) {
      return(1)
    } else {
      closest.x.index <- which.min(abs(x-kernel.est$x))
      before <- sum(kernel.est$y[1:closest.x.index]*1/kernel.granularity)
      return(before/area)
    }
  })
}

#
prior.prob.gtr = function(theta, cdf, polarity) {
  if (polarity == 'positive') {
    return(1-cdf(theta))
  } else {
    return(1 - prior.prob.gtr(theta, cdf, 'positive'))
  }
}

#erin did not change this fn
approx.equal = function(x,y) {return(abs(x - y) < .00001)}

#erin did not change this
utterance.index = function(utt) {return(which(possible.utterances == utt))}

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

#erin did not change this fn
relevant.theta = function(utterance) {return(which(possible.utterances[-1] == utterance))}

is.true = function(utterance, thetas, degree, thetaGtr) {
  if (thetaGtr) {
    theta.cond <- thetas[relevant.theta('very pos')] > thetas[relevant.theta('pos')]
  } else {
    theta.cond <- T
  }
	if (utterance == 'no-utt') {
		return(T)
	} else if (polarity(utterance) == 'positive' &&
               degree >= thetas[relevant.theta(utterance)] && theta.cond) {
		return(T)
	} else if (polarity(utterance) == 'negative' &&
               degree <= thetas[relevant.theta(utterance)]) {
		return(T)
	} else {
		return(F)
	}
}

#todo: put back erins Gtr test..
listener0 = function(utterance, thetas, degree, cdf, thetaGtr) {
  if (utterance == 'no-utt') {
    return(1)
  }	
  theta = thetas[relevant.theta(utterance)]
  if(polarity(utterance) == 'positive') {
    if(degree < theta) {
      return(0) #utterance false
    } else {
      return(1-cdf(theta))
    }
  } else {
    if(degree > theta) {
      return(0) #utterance false
    } else {
      return(cdf(theta))
    }
  }
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

speaker1 = function(thetas, degree, utterance, alpha, utt.cost, cdf, thetaGtr) {
	eval.utt = function(utt) {
		cost = 
		l0 = listener0(utt, thetas, degree, cdf, thetaGtr)
		return( (l0^alpha) * exp(-alpha * utt.cost *  mylnth(utt)))
	}
	utt.probs = sapply(possible.utterances, FUN=eval.utt)
	return(utt.probs[utterance.index(utterance)]/sum(utt.probs))
}

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size,
                     dist, band.width, thetaGtr) {
  kernel.est <- est.kernel(dist, band.width)
  dens <- make.pdf(kernel.est)
  cdf <- make.cdf(kernel.est)
    
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
	samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
  
  
  #cached likelihoods on a grid (currently hard-wired for two thetas):
  grid = seq(0,1,length.out=grid.steps)
  s1 = array(NA,dim = c(grid.steps,grid.steps,grid.steps))
  S1.likelihood = function(d,t1,t2) {
    #faster way to do this?
    d = which.min(abs(d-grid))
    t1 = which.min(abs(t1-grid))
    t2 = which.min(abs(t2-grid))
    if(is.na(s1[d,t1,t2])) {
      s1[d,t1,t2] <- speaker1(c(grid[t1], grid[t2]), grid[d], utterance, alpha, utt.cost, cdf, thetaGtr)
    }
    return(s1[d,t1,t2])
  }
  
  #scoring function, to compute (unormalized) probability of state. (should be in log domain?)
  prob.unnormed = function(state) {
    #check bounds:
    if (any(state < 0) || any(state > 1)) {return(0)}
    degree = state[1]
    thetas = state[2:length(state)]
    #prior for degree (thetas have unif prior):
    prior = dens(degree)
    #probbaility speaker would have said this (given state):
    likelihood = S1.likelihood(degree, thetas[1], thetas[2])
    return(prior*likelihood)
  }
  
  #initialize chain by rejection:
  print("initializing chain")
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
  
	return(list(samples=samples, prop.accepted=n.proposals.accepted/(n.samples-1)))
}

#run model with these values of parameters
model <- function(alpha, utt.cost, thetaGtr, label) {
  n.true.samples <- 10#30000 #number of samples to keep
  lag <- 10#50 #number of samples to skip over
  burn.in <- 10#5000
  n.samples <- n.true.samples * lag + burn.in
  step.size <- 0.005
  dists <- c("down", "mid", "unif")
  model.runs <- lapply(dists, function(dist) {
    return(lapply(possible.utterances, function(utterance) {
      listener1(utterance, alpha=alpha, utt.cost=utt.cost, n.samples=n.samples,
                step.size=step.size, dist=dist, band.width=0.09466942, thetaGtr=thetaGtr)
    }))
  })
  model.runs <- lapply(model.runs, function(run) {
    names(run) <- possible.utterances
    return(run)
  })
  names(model.runs) <- dists
  
  myapply <- function(f) {
    c(sapply(dists, function(dist) {
      return(c(sapply(possible.utterances, function(utterance) {
        return(f(dist, utterance))
      })))
    }))
  }
  
  graph.dist <- myapply(function(d,u){return(d)})
  graph.utterance <- myapply(function(d,u){return(u)})
  graph.means <- myapply(function(d,u){
    du.name <- paste(c(label, "-", d, "-", u, ".data"), collapse="")
    du.frame <- model.runs[[d]][[u]]
    #save all data
    write.table(du.frame, du.name)
    #get mean for this combo of dist and modifier
    return(mean(du.frame[["samples"]]))
  })
  
  avg.data <- data.frame(dist=graph.dist, utterance=graph.utterance, mean=graph.means)
  graph.data <- matrix(avg.data$mean, nrow=3, ncol=3, dimnames=list(possible.utterances, dists))
  png(paste(c(label, ".png"), collapse=""))
  barplot(graph.data, beside=T, main="Model", ylab="feppiness",
          col=rainbow(3), ylim=c(0,1))
  
  avg.data
  dev.off()
}

#graph data and save plot -- CHECK THIS!
sapply( c("down", "mid", "unif"), function(dist) {
  h <- 0.09
  kernel.est <- est.kernel(dist, h)
  dens <- make.pdf(kernel.est)
  int <- make.cdf(kernel.est)
  x <- seq(-1, 2, 0.01)
  int.y <- sapply(x, int)
  dens.y <- sapply(x, dens)
  plot(x,int.y,ylim=c(0,5), ylab="", xlab="", type="l")
  par(new=T)
  plot(x,dens.y, ylim=c(0,5), ylab="", xlab="", type="l")
  print(integrate(function(x){return(sapply(x, dens))}, lower=-0.1, upper=1.1))
})

#run the model with different values of free parameters
model(alpha=1, utt.cost=2, thetaGtr=T, label="output/alpha1cost2thetaGtr")
model(alpha=1, utt.cost=2, thetaGtr=F, label="output/alpha1cost2")
model(alpha=1, utt.cost=2, thetaGtr=F, label="output/alpha1cost5")
model(alpha=2, utt.cost=2, thetaGtr=T, label="output/alpha2cost2thetaGtr")
model(alpha=2, utt.cost=2, thetaGtr=F, label="output/alpha2cost2")
model(alpha=2, utt.cost=2, thetaGtr=F, label="output/alpha2cost5")
model(alpha=4, utt.cost=2, thetaGtr=T, label="output/alpha4cost2thetaGtr")
model(alpha=4, utt.cost=2, thetaGtr=F, label="output/alpha4cost2")
model(alpha=4, utt.cost=2, thetaGtr=F, label="output/alpha4cost5")