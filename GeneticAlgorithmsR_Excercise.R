# I.T.E.S.O.
# Management and Analysis of Massive Information - Spring 2022
# Homework 1: Genetic Algorithms
# Adrian Ramos Perez - Student of Master in Data Science program

#Load packages in memory:
# install.packages("genalg")
library(genalg)

# Restriction variable:
weight.limit <- 11.0
# Data frame definition:
items <- c("water","sleeping bag","tv","medicine","selfie stick","thermal clothes","maruchan","healthy snacks","Nintendo",
           "solar protector","hat","lantern","batteries","flipflops","boots"
           )
cost <- c(4.0, 0.8, 5.0, 0.3, 0.2, 0.75, 0.6, 0.9, 1.5, 0.3, 0.25, 0.35, 0.15, 0.1, 0.95)
benefit <- c(100L, 90L, 0L, 90L, 3L, 95L, 45L, 100L, 0L, 90L, 85L, 80L, 80L, 0L, 70L)

plan.desert <- data.frame(
  items = items,
  cost = cost,
  benefit = benefit
)
# Print dataset and its row number:
plan.desert
chromosome.size <- nrow(plan.desert)
chromosome.size

# Fitness function definition:
# Parameters: chromosome 'x'
# Outputs: 
fitness.generic <- function(x){
  
  items.cost <- x %*% plan.desert$cost
  items.benefit <- x %*% plan.desert$benefit
  if (items.cost > weight.limit)
  {
    return(1000*(items.cost-weight.limit))
  }
  else
  {
    return (-items.benefit)
  }
  
} # fitness func. end

# Candidate solutions' vectors definition:
# Candidate 1
# Candidate 2 decides not to take water nor boots with him because of the weight, a bad decision
# Candidate 3 takes only important things, leaving superficial and not so useful things
# Candidate 4 takes lantern without batteries
# Candidate 5 

candidate.1 <- c(1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
candidate.1 %*% plan.desert$cost
candidate.1 %*% plan.desert$benefit

candidate.2 <- c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
candidate.2 %*% plan.desert$cost
candidate.2 %*% plan.desert$benefit

candidate.3 <- c(1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1)
candidate.3 %*% plan.desert$cost
candidate.3 %*% plan.desert$benefit

candidate.4 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0)
candidate.4 %*% plan.desert$cost
candidate.4 %*% plan.desert$benefit

candidate.5 <- c(1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1)
candidate.5 %*% plan.desert$cost
candidate.5 %*% plan.desert$benefit

# Future improvement: develop all possible combinations of genes for a total of 2^15=32,768 different candidates]
  
fitness.generic(candidate.1)
fitness.generic(candidate.2)
fitness.generic(candidate.3)
fitness.generic(candidate.4)
fitness.generic(candidate.5)

# Population size in individuals: x times the chromosome size
population.size <- as.integer(chromosome.size*20)
clon.ind.percentage <- 0.20
# Genetic Algorithm
?rbga.bin
ga.tree <- rbga.bin(size = chromosome.size, popSize =  population.size,
                    mutationChance = .01,
                    elitism = as.integer(population.size * clon.ind.percentage),
                    iters = 30,
                    evalFunc = fitness.generic,
                    verbose = TRUE)
# Best result seems to be a descendant of candidate 3 but with the inclusion of selfie stick
summary(ga.tree, echo = T)
attributes(ga.tree)
ga.tree$population
ga.tree$evaluations
# Save the best chromosome / attributes
best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),][1,]
# Show attributes of the winner/best evolved chromosome:
plan.desert[best==1,]$items

# Comprobation of the fitness of the best solution obtained:
fitness.generic(best)
best %*% plan.desert$cost
best %*% plan.desert$benefit
