librarian::shelf(martinlaw/curtailment, rpact, gsDesign, clinfun, ggplot2)
# Original design?
x <- find2stageDesigns(nmin=23, nmax=27, p0=0.75, p1=0.92, alpha=0.22, power=0.95, benefit=T)

# June 2024: more designs. Continuous monitoring? No stopping for benefit.
# No designs with N=25 and alpha=0.2 (and stopping when CP=0.2):
des <- singlearmDesign(nmin = 10, nmax = 25, C = 1, p0=0.75, p1=0.92,
                alpha=0.2, power=0.95, progressBar = T, minthetaE = 1,
                maxthetaF = 0.2)

# Increase alpha, decrease CP threshold (trial ends if CP<0.1):
des <- singlearmDesign(nmin = 10, nmax = 25, C = 1, p0=0.75, p1=0.92,
                       alpha=0.22, power=0.95, progressBar = T, minthetaE = 1,
                       maxthetaF = 0.1)
des
des1 <- drawDiagram(des)
ggsave("morgan_04062024.png", bg = "white")

# Try other packages: rpact: https://www.rpact.org/vignettes/planning/rpact_boundary_examples/
# Example: non-binding futility boundary using an O'Brien & Fleming type
# beta spending function. No early stopping for efficacy (i.e., all alpha
# is spent at the final analysis).
design <- getDesignGroupSequential(
  sided = 1, alpha = 0.025, beta = 0.05,
  informationRates = c(0.33, 0.67, 1),
  typeOfDesign = "noEarlyEfficacy",
  typeBetaSpending = "bsOF",
  bindingFutility = TRUE
)
# Does not seem to be possible to request continuous monitoring (max number of stages=20).

# Try package gsDesign2: does not cover single arm designs

# Try package gsDesign:
# ?gsDesign::gsBinomialExact() # Need to specify number of interims.

# Try package clinfun:
#clinfun::gsdesign.binomial
# Seems again to not cover single-arm designs.

# Finally: test design using simulation:
n <- 25
p0 <- 0.75
p1 <- 0.92
nsims <- 100000
output <- vector("list", nsims)
for(i in 1:nsims){
  single.h0 <- rbinom(n=n, size=1, prob=p0)
  # Does trial stop for futility?
  fail.h0 <- any(cumsum(single.h0) <= des1$bounds.mat$fail)
  single.h1 <- rbinom(n=n, size=1, prob=p1)
  # Does trial stop for futility?
  fail.h1 <- any(cumsum(single.h1) <= des1$bounds.mat$fail)
  output[[i]] <- c(fail.h0, fail.h1)
}
output <- do.call(rbind, output)
success.propn <- 1-(colSums(output)/nsims)
round(success.propn, 2)
