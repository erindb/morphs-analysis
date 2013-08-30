setwd("~/morphs-analysis/")  ###change this to actual location of repo

library(stats)

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
prior.prob.gtr = function(theta, int, polarity) {
  if (polarity == 'positive') {
    return((int(1) - int(theta))/(int(1) - int(0)))
  } else {
    return(1 - prior.prob.gtr(theta, int, 'positive'))
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

listener0 = function(utterance, thetas, degree, int, thetaGtr) {
	if (!is.true(utterance, thetas, degree, thetaGtr)) {
		return(0)
	} else if (utterance == 'no-utt') {
		return(1)
	} else {		
# return the prior probability of the utterance being true, as long as it isn't 0. 	
		return(1/prior.prob.gtr(thetas[relevant.theta(utterance)], int, polarity(utterance)))  
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

speaker1 = function(thetas, degree, alpha, utt.cost, int, thetaGtr) {
	eval.utt = function(utt) {
		prior = exp(-alpha * utt.cost)^mylnth(utt)
		likelihood = exp(alpha * log(listener0(utt, thetas, degree, int, thetaGtr)))
		return(prior * likelihood)
	}
	utt.probs = sapply(possible.utterances, FUN=eval.utt)
	return(utt.probs/sum(utt.probs))
}

#erin did not change this fn
admissible = function(vec) {
  if (any(vec < 0) || any(vec > 1)) {
    return(F)
  } else if (length(vec) <= 3) {
    return(T)
  } else {
    cons = function(i) {return(vec[i] >= vec[i-1])}
    neg.indices = 2:(1 + (length(vec) - 1)/2)
    pos.indices = (2 + (length(vec) - 1)/2):length(vec)
    if (!all(sapply(neg.indices[-1], cons))) {
      return(F)
    } else if (!all(sapply(pos.indices[-1], cons))) {
      return(F)
     } else {
       return(T)
    }
  }
}

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size,
                     dist, band.width, num.adj.pairs=1, mods=F,
                     thetaGtr) {
  kernel.est <- est.kernel(dist, band.width)
  dens <- make.pdf(kernel.est)
  int <- make.cdf(kernel.est)
  
  flip = function(p) {return(runif(1,0,1) < p)}
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
	samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
	sample.prob.NN = 0
	while (approx.equal(sample.prob.NN,0)) {
	  sample.degree.prior = 0
    while (sample.degree.prior == 0) {
  		sample.degree = runif(1,0,1)
	  	sample.degree.prior = dens(sample.degree)
    }
    sample.thetas = runif(length(possible.utterances) - 1, 0, 1) 
		while (!admissible(c(sample.degree, sample.thetas))) {
			sample.thetas = runif(length(possible.utterances) - 1, 0, 1)
		}
		sample.likelihood = speaker1(sample.thetas, sample.degree,
                                 alpha, utt.cost, int, thetaGtr)[utterance.index(utterance)]
		sample.prob.NN = sample.degree.prior * sample.likelihood
	}
	sample.vec = c(sample.degree, sample.thetas)
	samples[1,] = sample.vec
	n.proposals.accepted = 0
	increment = function(n) {if (n < length(sample.vec)) {return(n+1)} else {return(1)}}
	switch = 0
	make.proposal = function(v, switch) {
		proposal.v = v
		proposal.v[switch] = sample(c(proposal.v[switch] + step.size, proposal.v[switch] - step.size), 1)
		return(proposal.v)
	}
	for (i in 2:n.samples) {
		switch = increment(switch)
		proposal.vec = make.proposal(sample.vec, switch)
 		if (admissible(proposal.vec)) {
			proposal.degree = proposal.vec[1]
			proposal.thetas = proposal.vec[2:length(proposal.vec)]
			proposal.degree.prior = dens(proposal.degree)
			proposal.likelihood = speaker1(proposal.thetas, proposal.degree, alpha, utt.cost, int, thetaGtr)[utterance.index(utterance)]
			proposal.prob.NN = proposal.degree.prior * proposal.likelihood
			accept = flip(min(1, proposal.prob.NN/sample.prob.NN))
			if (accept) {
				n.proposals.accepted = n.proposals.accepted + 1
			 	sample.vec = proposal.vec
			 	sample.prob.NN = proposal.prob.NN
			} 
		}
		samples[i,] = sample.vec
	}
	return(list(samples=samples, prop.accepted=n.proposals.accepted/(n.samples-1)))
}

#run model with these values of parameters
model <- function(alpha, utt.cost, thetaGtr, label) {
  n.true.samples <- 30000 #number of samples to keep
  lag <- 50 #number of samples to skip over
  burn.in <- 5000
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
    return(mean(du.frame[["samples"]][,"degree"]))
  })
  write.table(graph.means, paste(c(label, "-means.data"), collapse=""))
  graph.data <- (matrix(data=graph.means, nrow=3, ncol=3,
                        dimnames=list(c("none", "adj", "very"),
                                      c("peakedDown", "peakedMid", "uniform"))))
  png(paste(c(label, ".png"), collapse=""))
  novel.adj.bar <- barplot(as.matrix(graph.data), main="alpha=1, cost=2, very>pos",
                           ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
  legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
  dev.off()
}

timestamp <- as.character(unclass(Sys.time()))

mainDir <- "~/morphs-analysis/"
subDir <- paste(c("output", timestamp), collapse="")

if (!(file.exists(subDir))) {
  dir.create(file.path(mainDir, subDir))
}

time.label <- function(identifier) {
  return(paste(c("output", timestamp, "/", timestamp, identifier), collapse=""))
}

#run the model with different values of free parameters
sapply(1:10, function(i) {
  model(alpha=1,utt.cost=1,thetaGtr=F,label=time.label("#alpha1cost1"))
})
