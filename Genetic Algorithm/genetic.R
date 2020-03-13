#general parameters
numberOfVariables = 0
numberOfClauses = 0
listOfClauses = list()

#GA parameters
populationSize = 100
currentPopulationSize = 100
maximumGeneration = 1000
mutationProbability = 0.02
initialMutationProbability = 0.02
hypermutationProbability = 0.4
generationsForHypermutation = 50
crossoverProbability = 0.3
bestCandidate = NA


process_file <- function(filepath)
{
  content = file(filepath, "r")
  
  line = readLines(content, n = 1)
  while (line[1] == 'c')
  {
    line = readLines(content, n = 1)
  }
  
  listOfArgs = unlist(strsplit(line[1], split = "\\s"))
  numberOfVariables <<- as.numeric(listOfArgs[3])
  numberOfClauses <<- as.numeric(listOfArgs[4])
  
  currentClause = 1
  
  while (1)
  {
    line = readLines(content, n = 1)
    if (length(line) == 0)
    {
      break
    }    
    else
    {
      clause = c()
      numOfVars = 1
      
      listOfArgs = unlist(strsplit(line[1], split = "\\s"))
      
      while (1)
      {
        var = as.numeric(listOfArgs[numOfVars])
        if (var == 0 || is.na(var))
        {
          break;
        }
        clause = c(clause, var)
        numOfVars = numOfVars + 1
      }
      
      listOfClauses[[currentClause]] <<- clause
      currentClause = currentClause + 1
    }
  }
  close(content)
}

evaluate_clause <- function(assign, index)
{
  for(i in 1:length(listOfClauses[[index]]))
  {
    if (listOfClauses[[index]][i] < 0)
    {
      if (assign[(-1) * listOfClauses[[index]][i]] == 0)
      {
        return (1)
      }
    }
    else
    {
      if (assign[listOfClauses[[index]][i]] == 1)
      {
        return (1)
      }
    }
  }
  return (0)
}

eval <- function(candidate)
{
  numberOfTrueClauses = 0
  for (i in 1:numberOfClauses)
  {
    numberOfTrueClauses = numberOfTrueClauses + evaluate_clause(candidate, i)
  }
  
  return (numberOfTrueClauses)
}

generate_candidate <- function() 
{
  candidate = c()
  candidate = rbinom(numberOfVariables, 1, 0.5)
  return (candidate)
}

generate_population <- function() 
{
  i = 1
  population = matrix(NA, nrow = 2 * populationSize, ncol = numberOfVariables)
  while (i <= populationSize) 
  {
    candidate = generate_candidate()
    population[i, ] = candidate
    i = i + 1
  }
  return (population)
}

mutation <- function(population) 
{
  for(i in 1:populationSize)
  {
    for(j in 1:numberOfVariables)
    {
      probability = runif(1, 0, 1)
      if (probability < mutationProbability) 
      {
        population[i, j] = 1 - population[i, j]
      }
    }
  }
  
  return (population)
}

generate_crossover_probabilities <- function() 
{
  probabilities = c()
  i = 0
  while (i < populationSize) 
  {
    randomProbability = runif(1, 0, 1)
    probabilities = c(probabilities, randomProbability)
    i = i + 1
  }
  return (probabilities)
}

one_cut_point_crossover <- function(parent1, parent2) 
{
  cutPoint = sample(2:(numberOfVariables - 1), 1)
  child1 = c()
  child2 = c()
  
  i = 1
  while (i <= cutPoint) 
  {
    child1 = c(child1, parent1[i])
    child2 = c(child2, parent2[i])
    i = i + 1
  }
  
  while (i <= numberOfVariables) 
  {
    child1 = c(child1, parent2[i])
    child2 = c(child2, parent1[i])
    i = i + 1
  }
  
  return (list(child1 = child1, child2 = child2))
}

crossover <- function(population) 
{
  probabilities = generate_crossover_probabilities()
  position = seq(from = 1, to = populationSize, by = 1)
  
  #sort probabilities ascending
  for (i in 1:(populationSize - 1)) 
  {
    for (j in (i + 1):populationSize) 
    {
      if (probabilities[i] > probabilities[j]) 
      {
        aux = probabilities[i]
        probabilities[i] = probabilities[j]
        probabilities[j] = aux
        
        aux = position[i]
        position[i] = position[j]
        position[j] = aux
      }
    }
  }
  
  i = 1
  currentMember = populationSize + 1
  
  while (i <= populationSize - 1 && probabilities[i + 1] < crossoverProbability && probabilities[i] < crossoverProbability) 
  {
    children = one_cut_point_crossover(population[position[i], ], population[position[i + 1], ])
    population[currentMember, ] = children[['child1']]
    population[currentMember + 1, ] = children[['child2']]
    currentMember = currentMember + 2
    i = i + 2
  }
  
  if (i <= populationSize - 1 && probabilities[i] < crossoverProbability) 
  {
    randomProbability = runif(1, 0, 1)
    if (randomProbability < 0.5) 
    {
      children = one_cut_point_crossover(population[position[i], ], population[position[i + 1], ])
      population[currentMember, ] = children[['child1']]
      population[currentMember + 1, ] = children[['child2']]
      currentMember = currentMember + 2
    }
  }
  
  currentPopulationSize <<- currentMember - 1
  return (population)
}

evaluate_population <- function(population)
{
  fitnessValues = c()
  for (i in 1:currentPopulationSize)
  {
    fitnessValues = c(fitnessValues, eval(population[i, ]))
  }
  
  return (fitnessValues)
}

worst_candidate <- function(fitnessValues)
{
  worst = fitnessValues[1]
  for (i in 1:length(fitnessValues)) 
  {
    if (worst > fitnessValues[i]) 
    {
      worst = fitnessValues[i]
    }
  }
  
  return (worst)
}

best_candidate <- function(fitnessValues) 
{
  best = fitnessValues[1]
  for (i in 1:length(fitnessValues)) 
  {
    if (best < fitnessValues[i]) 
    {
      best = fitnessValues[i]
    }
  }
  
  return (best)
}

best_candidate_index <- function(fitnessValues) 
{
  best = fitnessValues[1]
  index = 1
  for (i in 1:length(fitnessValues)) 
  {
    if (best < fitnessValues[i])
    {
      best = fitnessValues[i]
      index = i
    }
  }
  
  return (index)
}

select <- function(partialSums) 
{
  randomPosition = runif(1, 0, 1)
  randomPosition = randomPosition * partialSums[length(partialSums)]
  i = 1
  while (i <= length(partialSums)) 
  {
    if (randomPosition <= partialSums[i]) 
    {
      return (i)
    }
    i = i + 1
  }
}

selection <- function(population, fitnessValues) 
{
  partialSums = c(fitnessValues[1])
  for (i in 2:length(fitnessValues)) 
  {
    partialSums = c(partialSums, partialSums[i - 1] + fitnessValues[i])
  }
  
  newPopulation = matrix(NA, nrow = 2 * populationSize, ncol = numberOfVariables)
  
  bestCandidateValue = eval(bestCandidate)
  
  if (bestCandidateValue < best_candidate(fitnessValues))
  {
    bestCandidate <<- population[best_candidate_index(fitnessValues), ]
  }
  
  for (i in 1:populationSize) 
  {
    randomNum = runif(1, 0, 1)
    if (randomNum < 0.05) 
    {
      newPopulation[i, ] = bestCandidate
    }
    else 
    {
      newPopulation[i, ] = population[select(partialSums), ]
    }
  }
  
  return (newPopulation)
}


GA <- function()
{
  generationsResults = c()
  population = generate_population()
  fitnessValues = evaluate_population(population)
  bestCandidate <<- population[best_candidate_index(fitnessValues), ]
  
  for (numberOfGenerations in 1:maximumGeneration) 
  {
    cat(numberOfGenerations, ' ', eval(bestCandidate), '\n')
    
    generationsResults = c(generationsResults, eval(bestCandidate))
    if (numberOfGenerations > 100 && generationsResults[numberOfGenerations] == generationsResults[numberOfGenerations - 100])
    {
      return(eval(bestCandidate))
    }
    #hypermutation
    if (numberOfGenerations > generationsForHypermutation &&
        generationsResults[numberOfGenerations] == generationsResults[numberOfGenerations - generationsForHypermutation]) 
    {
      mutationProbability <<- hypermutationProbability
    }
    else
    {
      mutationProbability <<- initialMutationProbability
    }
    
    population = mutation(population)
    population = crossover(population)
    fitnessValues = evaluate_population(population)
    population = selection(population, fitnessValues)
  }
  
  return(eval(bestCandidate))
}

best_improvement_hill_climbing <- function(bestCandidate) 
{
  currentBest = bestCandidate
  currentBestFitness = eval(currentBest)
  improvement = TRUE
  while (improvement == TRUE) 
  {
    currentNeighbor = currentBest
    for (i in 1:numberOfVariables) 
    {
      currentNeighbor[i] = 1 - currentNeighbor[i]
      currentNeighborFitness = eval(currentNeighbor)
      
      if (currentNeighborFitness > currentBestFitness)
      {
        currentBest = currentNeighbor
        currentBestFitness = currentNeighborFitness
      }
    }
    
    if (currentBestFitness > eval(bestCandidate)) 
    {
      bestCandidate = currentBest
      cat(eval(bestCandidate), '\n')
    }
    else 
    {
      improvement = FALSE
    }
  }
  return(eval(bestCandidate))
}


filepath = 'instances/frb40-19-cnf/frb40-19-1.cnf'
process_file(filepath)

result = GA()
finalResult = best_improvement_hill_climbing(bestCandidate)

dataFile = "data_GA.txt"
write(finalResult, dataFile, append=TRUE)


