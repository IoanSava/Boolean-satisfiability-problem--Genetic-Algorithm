#general parameters
numberOfVariables = 0
numberOfClauses = 0
listOfClauses = list()

#sa parameters
INITIAL_TEMPERATURE = 100
COOLING_RATE = 0.9
ITERATIONS = 200

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


simulated_annealing <- function()
{
  iterationsResults = c()
  best = generate_candidate()
  bestValue = eval(best)
  
  temperature = INITIAL_TEMPERATURE
  
  for (i in 1:ITERATIONS)
  {
    currentCandidate = c()
    if (i == 1)
    {
      currentCandidate = best
    }
    else
    {
      currentCandidate = generate_candidate()
    }
    
    currentCandidateValue = eval(currentCandidate)
    currentBestCandidate = currentCandidate
    currentBestCandidateValue = currentCandidateValue
    
    improvement = TRUE
    while (improvement == TRUE)
    {
      currentNeighbor = c()
      for (j in 1:numberOfVariables)
      {
        if (j == 1)
        {
          currentNeighbor = currentCandidate
        }
        else
        {
          currentNeighbor[j - 1] = 1 - currentNeighbor[j - 1]
        }
        
        currentNeighbor[j] = 1 - currentNeighbor[j]
        currentNeighborValue = eval(currentNeighbor)
        
        if (currentNeighborValue > currentBestCandidateValue)
        {
          currentBestCandidateValue = currentNeighborValue
          currentBestCandidate = currentNeighbor
        }
        else
        if (runif(1, 0, 1) < exp((-1 * abs(currentNeighborValue - currentBestCandidateValue)) / temperature))
        {
          currentBestCandidateValue = currentNeighborValue
          currentBestCandidate = currentNeighbor
        }
      }
      
      if (currentBestCandidateValue > currentCandidateValue)
      {
        currentCandidate = currentBestCandidate
        currentCandidateValue = currentBestCandidateValue
      }
      else
      {
        improvement = FALSE
      }
    }
    
    if (currentCandidateValue > bestValue)
    {
      best = currentCandidate
      bestValue = currentCandidateValue
    }
    
    temperature = temperature * COOLING_RATE
    
    cat(i, ' ', bestValue, '\n')
    iterationsResults = c(iterationsResults, bestValue)
    if (i > 10)
    {
      if (iterationsResults[i] == iterationsResults[i - 10])
      {
        return (bestValue)
      }
    }
  }
  
  return (bestValue)
}

filepath = 'instances/frb50-23-cnf/frb50-23-1.cnf'
process_file(filepath)

result = simulated_annealing()

dataFile = "data_SA.txt"
write(result, dataFile, append=TRUE)
