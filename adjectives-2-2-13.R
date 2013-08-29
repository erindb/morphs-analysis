#priors: 
	# 1 is uniform
	# 2 is beta(.5, .5) (peaked at both edges)
	# 3-4 are neg skewed with zero at 1
	# 5-6 are neg skewed with non-zero probability at zero 
	# 7 is normal-ish in the middle of the scale, with zero prob at edges
	# 8 is normal-ish in the middle with non-zero prob at edges
	# 9-12 are like 3-6, but inverted scales. check that they are indeed the same.

possible.utterances = c()
integral = function(prior.type) {
  integrals = list(
    function(x) {return(x)}, #1
    NA, #2
    function(x) {return(-3/2 * (x-2) * x)}, #3
    function(x) {return(-1/33 * (3-3*x)^11)}, #4
    function(x) {return(1/2 * (8 - 3*x) * x)}, #5
    function(x) {return(-1/18 * (4 - 3*x)^6)}, #6
    NA, #7
    NA, #8
    function(x) { #9
      if (x < .05) {
        return(.45 * x/.05)
      } else if (x >= .05 && x < .475) {
        return(.45 + .1 * (x - .05))
      } else if (x >= .475 && x < .525) {
        return(.5 + .475 * (x - .475)/.05)
      } else {
        return(1)
      }
    }
  )
  return(integrals[[prior.type]])
}
calc.NC = function(prior.type) {
  f = integral(prior.type)
  return(f(1) - f(0))
}
densfxn = function(prior.type) {
  densfxns = list(
    function(x) {return(1)}, #1
    NA, #2
    function(x) {return(3-3*x)}, #3
    function(x) {return((3 - 3*x)^10)}, #4
    function(x) {return(4 - 3 * x)}, #5
    function(x) {return((4 - 3*x)^5)}, #6
    NA, #7
    NA, #8
    function(x) { #9
      if (x < .05 || (x >= .475 && x < .525)) {
        return(.9)
      } else if (x >= .05 && x < .475) {
        return(.0425)
      } else {
        return(0)
      }
    } 
  )
  return(densfxns[[prior.type]])
} 
# substitute for all cases of  fxn find.density
normalized.density.fxn = function(prior.type) {
  if (prior.type == 2) {
    return(function(x) {return(dbeta(x, .5, .5))})
  } else if (prior.type == 7) {
    # 0.9991419 is the prob mass between 0 and 1 for this gaussian
    return(function(x) {return(dnorm(x, .5, .15)/0.9991419)})
  } else if (prior.type == 8) {
  	# 0.9522094 is the prob mass between 0 and 1 for this gaussian
    return(function(x) {return(dnorm(x, .25, .15)/0.9522094)})
  } else {
    return(function(x) {return(densfxn(prior.type)(x)/calc.NC(prior.type))})
  }
}

prior.prob.gtr = function(theta, prior.type, polarity) {
  if (polarity == 'positive') {
    if (prior.type == 2) {
      return(pbeta(theta, .5, .5, lower.tail=F))
    } else if (prior.type == 7) {
      mu = .5
      sigma = .15
      NC1 = pnorm(1, mu, sigma) - pnorm(0, mu, sigma)
      return((pnorm(1, mu, sigma) - pnorm(theta, mu, sigma))/NC1)
    } else if (prior.type == 8) {
      mu = .25
      sigma = .15
      NC2 = pnorm(1, .25, .15) - pnorm(0, .25, .15)
      return((pnorm(1, mu, sigma) - pnorm(theta, mu, sigma))/NC2)
    } else {
      int = integral(prior.type)
      return((int(1) - int(theta))/(int(1) - int(0)))
    }
  } else {
    return(1 - prior.prob.gtr(theta, prior.type, 'positive'))
  }
}

set.possible.utterances = function(num.adj.pairs, mods=F) {
  if (num.adj.pairs == .5) {
    possible.utterances <<- c('no-utt', 'pos')
    return(T)
  } else if (num.adj.pairs == 1 && !mods) {
		possible.utterances <<- c('no-utt', 'neg', 'pos')
		return(T)
	} else if (num.adj.pairs == 2 && !mods) {
		possible.utterances <<- c('no-utt', 'neg2', 'neg1', 'pos1', 'pos2')
		return(T)
	} else if (num.adj.pairs == 3 && !mods) {
		possible.utterances <<- c('no-utt', 'neg3', 'neg2', 'neg1', 'pos1', 'pos2', 'pos3')
		return(T)
	} else if (num.adj.pairs == 1 && mods) {
		possible.utterances <<- c('no-utt', 'very neg', 'neg', 'pos', 'very pos')
		return(T)
	} else {
		print("Error at function set.possible.utterances: unknown number of pairs")
		return(F)
	}
}

approx.equal = function(x,y) {return(abs(x - y) < .00001)}

utterance.index = function(utt) {return(which(possible.utterances == utt))}

polarity = function(utterance) {
	if (utterance == 'pos' || utterance == 'very pos' || utterance == 'pos1' || utterance == 'very pos1' || utterance == 'pos2' || utterance == 'pos3') {
		return('positive')
	} else if (utterance == 'neg' || utterance == 'very neg' || utterance == 'neg1' || utterance == 'very neg1' || utterance == 'neg2' || utterance == 'neg3') {
		return('negative')
	} else {
		print("Error in function polarity: unknown utterance")
	}
}

relevant.theta = function(utterance) {return(which(possible.utterances[-1] == utterance))}

is.true = function(utterance, thetas, degree) {
	if (utterance == 'no-utt') {
		return(T)
	} else if (polarity(utterance) == 'positive' && degree >= thetas[relevant.theta(utterance)]) {
		return(T)
	} else if (polarity(utterance) == 'negative' && degree <= thetas[relevant.theta(utterance)]) {
		return(T)
	} else {
		return(F)
	}
}

listener0 = function(utterance, thetas, degree, prior.type) {
	if (!is.true(utterance, thetas, degree)) {
		return(0)
	} else if (utterance == 'no-utt') {
		return(1)
	} else {		
# return the prior probability of the utterance being true, as long as it isn't 0. 	
		return(1/prior.prob.gtr(thetas[relevant.theta(utterance)], prior.type, polarity(utterance)))  
	}
} 

mylnth = function(u) {
	if (u == 'no-utt') {
		return(0)
	} else if (u=='very pos' || u=='very neg') {
		return(2)
	} else {
		return(1)
	}
}

speaker1 = function(thetas, degree, alpha, utt.cost, prior.type) {
	eval.utt = function(utt) {
		prior = exp(-alpha * utt.cost)^mylnth(utt)
		likelihood = exp(alpha * log(listener0(utt, thetas, degree, prior.type)))
		return(prior * likelihood)
	}
	utt.probs = sapply(possible.utterances, FUN=eval.utt)
	return(utt.probs/sum(utt.probs))
}

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

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size, prior.type, num.adj.pairs=1, mods=F) {
	pu.check = set.possible.utterances(num.adj.pairs, mods)
	if (!pu.check) {
		print("Error in listener1: unable to set possible utterances")
	}
	flip = function(p) {return(runif(1,0,1) < p)}
	samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=list(paste('samp', 1:n.samples, sep=''), c('degree', paste('theta.', possible.utterances[-1], sep=''))))
	sample.prob.NN = 0
  dens = normalized.density.fxn(prior.type)
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
		sample.likelihood = speaker1(sample.thetas, sample.degree, alpha, utt.cost, prior.type)[utterance.index(utterance)]
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
			proposal.likelihood = speaker1(proposal.thetas, proposal.degree, alpha, utt.cost, prior.type)[utterance.index(utterance)]
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

sim = function(prior.type, num.adj.pairs=1, step.size=.001, mods=F, plot=T, n.true.samples = 30000, burn.in=10000, lag=50) {
  pu.check = set.possible.utterances(num.adj.pairs, mods)
  if (!pu.check) {
    print("Error in sim: unable to set possible utterances")
  }
  n.samples = burn.in + n.true.samples * lag
	select.indices = round(seq(from=(burn.in+1), to=n.samples, by=lag))
	#alphas = c(2, 4)
	#utt.costs = c(1.5, 3)
  alpha = 4
  utt.cost = 2
  f.sim = function(utt) {return(listener1(utt, alpha, utt.cost, n.samples, step.size, prior.type, num.adj.pairs, mods)$samples[select.indices,])}
	sim.results = lapply(possible.utterances, FUN=f.sim)
	if (plot) {
    if (num.adj.pairs == .5) {par(mfrow=c(1,1))}
	  else {par(mfrow=c(2,((length(possible.utterances) - 1)/2)))}
		cols = c('blue', 'green', 'red', 'black', 'orange', 'purple', 'brown')
    for (i in 2:length(possible.utterances)) {
  		plot(density(sim.results[[i]][,1], bw='SJ'), lwd=3, col='blue', main=paste('utt = "', possible.utterances[[i]], '", prior=', prior.type, sep=''), xlab='Degree', ylab='Density', xlim=c(0,1), ylim=c(0,6), xaxs='i')
			lines(density(sim.results[[i]][,i], bw='SJ'), lwd=2, col='red')
  		legend('topright', legend=c('prior estimate', 'posterior estimate', 'theta posterior'), text.col=c('black', 'blue', 'red'), cex=.8)
			if (prior.type == 1) {
				abline(a=1, b=0, lwd=3, lty=3, col='black')
			} else if (prior.type != 9) {
        dens = normalized.density.fxn(prior.type)
				curve(dens(x), lty=3, add=T, col='black', lwd=2)
			}
    }
	}
	return(sim.results)
}

sim.pos.only = function(i) {return(sim(i, num.adj.pairs = .5, n.true.samples=30000, step.size=.005, lag=50, burn.in=5000))}

pos.only.gaussian = sim.pos.only(7)

sim.one.pair = function(i) {return(sim(i, n.true.samples=30000, step.size=.005, lag=500, burn.in=5000, plot=F))}

gaussian.prior.7.one.pair.sim = sim.one.pair(7)

# uniform = 1
# beta(.5,.5) = 2
# gaussian = 7

gaussian.pos.only.one.pair.alpha.1 = listener1('pos', alpha=1, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=7)

gaussian.pos.only.one.pair.alpha.2 = listener1('pos', alpha=2, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=7)

uniform.pos.only.one.pair.alpha.1 = listener1('pos', alpha=1, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=1)

uniform.pos.only.one.pair.alpha.2 = listener1('pos', alpha=2, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=1)

uniform.pos.only.one.pair.alpha.4 = listener1('pos', alpha=4, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=1)

doublepeaked.pos.only.one.pair.alpha.1 = listener1('pos', alpha=1, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=2)

doublepeaked.pos.only.one.pair.alpha.2 = listener1('pos', alpha=2, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=2)

doublepeaked.pos.only.one.pair.alpha.4 = listener1('pos', alpha=4, utt.cost=2, n.samples=30000*50+5000, step.size = .005, prior.type=4)

sq = seq(from=5001, to=30000*50+5000, by=50)

job.talk.plot = function(property.name, theta.data, degree.data, x1, y1, x2, y2, prior, savefiles=F, ymax=7) {
  if (!savefiles) {par(mfrow=c(2,2))}
  for (i in 1:4) {
    if (savefiles) {png(paste('~/Dropbox/Stanford\ job\ talk/', property.name, '-', i, '.png', sep=''), width = 600, height = 600)}
    plot(1,1,col='white', xlim=c(0,1), ylim=c(0, ymax), xlab=property.name, ylab='Probability density', xaxt='n', xaxs='i', cex.axis=2)
    
    if (prior == 1) {
    	abline(a=.8, b=0, col='red', lty=4, lwd=4)
    	text(.17, .5, expression(paste('Interp. prior, p(', theta, ')')), cex=1.5, col='red')
    } else {
    	abline(a=1, b=0, col='red', lty=4, lwd=4)
    	text(.15, 1.3, expression(paste('Interp. prior, p(', theta, ')')), cex=1.5, col='red')
    }
    
    if (i >= 2) {
      if (prior == 7) {curve(dnorm(x, .5, .15), col='black', add=T, lwd=4)}
      else if (prior == 1) {abline(a=1.2, b=0, col='black', lwd=4)}
      if (prior == 1) {
      	text(.18, 1.6, paste(property.name, 'prior, p(f)'), cex=1.5)	
      } else {
      	text(.3, 2.5, paste(property.name, 'prior'), cex=1.5)
      	text(.3, 2.1, 'p(h)', cex=1.5)
      }
    }
    if (i >= 3) {
      lines(density(theta.data), col='blue', lwd=4, lty=4)
      text(x1, y1, paste('Interp. posterior'), col='blue', cex=1.5)
      text(x1, y1-.4, expression(paste('p(', theta, '|u)')), col='blue', cex=1.5)
    }
    if (i == 4) {
      lines(density(degree.data), col='brown', lwd=4)
      text(x2, y2, property.name, col='brown', cex=1.5)
      text(x2, y2-.4, 'posterior', col='brown', cex=1.5)
      if (prior == 1) {text(x2, y2-.8, paste('p(f|u)'), col='brown', cex=1.5)}
      else {text(x2, y2-.8, paste('p(h|u)'), col='brown', cex=1.5)}
    }
    if (savefiles) {
      dev.off()
    }
  }
}
job.talk.plot('Height', gaussian.prior.7.one.pair.sim[[3]][,3], gaussian.prior.7.one.pair.sim[[3]][,1], .58, 6.5, .92, 4.5, prior = 7, savefiles=T)
job.talk.plot('Fullness', uniform.pos.only.one.pair.alpha.4[[1]][,3], uniform.pos.only.one.pair.alpha.4[[1]][,1], .72, 5, .87, 9, ymax = 10, prior = 1, savefiles=T)




uniform.pos.only.one.pair.alpha.4[[1]][,3]
sample.cond.prob = function(s, epsilon, eval) {
	cond = eval[which(eval < s)]
	cond.prob = function(x) {return(length(which(cond < x - epsilon))/length(cond))}
	return(cond.prob(s))
}
f = function(x) {return(sample.cond.prob(x, .01, uniform.pos.only.one.pair.alpha.4[[1]][sq,3]))}
res = sapply(sample(uniform.pos.only.one.pair.alpha.4[[1]][sq,1], 10000, replace=T), f)

# eval: uniform.pos.only.one.pair.alpha.4[[1]][sq,3]
f2 = function(x) {return(sample.cond.prob(x, .01, gaussian.prior.7.one.pair.sim[[3]][,3]))}
gauss.res = sapply(sample(gaussian.prior.7.one.pair.sim[[3]][,1], 10000, replace=T), f2)