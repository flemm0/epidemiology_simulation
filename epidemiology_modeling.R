##Function to set up the grid
#paramters are size of population and fraction of vaccinated individuals
createGrid = function(dimensions, fraction){
  pop = (dimensions[1]*dimensions[2])
  num2s = pop*fraction #size of population times fraction of vaccinated individuals
  num1s = pop - num2s #rest of population is unvaccinated
  popvec = c() #initialize vector to store number of individuals in the population
  for(i in 1:num2s){
    popvec = c(popvec, 2)
  }
  for(i in 1:num1s){
    popvec = c(popvec, 1)
  }
  
  popvec1 = sample(popvec, size = pop)
  
  arr = array(data=popvec1, dim=dimensions) #creates array from population vector with individuals randomly distributed
  return(arr)
}

#test code
print(createGrid(c(6,4), 0.75)) #create a 6x4 grid with 18 2s and six 1s randomly distributed


##Function to check whether two arrays are identical
areIdentical = function(arr1, arr2){
  flag = TRUE
  x = dim(arr1)
  y = dim(arr2)
  for(i in 1:length(x)){ #case to check if arrays are same size
    if(x[i] != y[i]){
      flag = FALSE
      break
    }
  }
  
  for(i in 1:length(arr1)){ #checks values in both arrays
    if(arr1[i] != arr2[i]){
      flag = FALSE
      break
    }
  }
  
  return(flag)
}

#Test code
testGrid1 = array(data = c(numeric(23), 1), dim = c(6, 4))
testGrid2 = testGrid1
print(areIdentical(testGrid1, testGrid2)) #TRUE
testGrid2 = array(data = c(numeric(10), 1, numeric(13)), dim = c(6, 4))
print(areIdentical(testGrid1, testGrid2)) #FALSE
testGrid3 = array(data=1:6, dim = c(3,2))
testGrid4 = array(data=1:6, dim = c(2,3))
print(areIdentical(testGrid3, testGrid4)) #FALSE
testGrid5 = array(data=c(3,2,4,4,5,6), dim = c(2,3))
print(areIdentical(testGrid5, testGrid4)) #FALSE



##function to find the coordinates of all neighbors of a given spot in an array
findNeighbors = function(inputarr, position){
  dimensions = dim(inputarr)
  rownum = position[1]
  colnum = position[2]
  vec1 = c() #vector to store row positions of all neighbors
  vec2 = c() #vector to store column positions of all neighbors
  if(colnum != 1){ #if statements to check that the position of individual is not at the edge of the grid
    vec1 = c(vec1, rownum)
    vec2 = c(vec2, colnum - 1)
  }
  if(colnum != dimensions[2]){
    vec1 = c(vec1, rownum)
    vec2 = c(vec2, colnum + 1)
  }
  if(rownum != 1){
    vec1 = c(vec1, rownum - 1)
    vec2 = c(vec2, colnum)
  }
  if(rownum != dimensions[1]){
    vec1 = c(vec1, rownum + 1)
    vec2 = c(vec2, colnum)
  }
  
  vec3 = c(vec1, vec2) #concatenate row and col vectors together
  outputarr = array(data=vec3, dim=c((length(vec3)/2),2)) #return data as array
  return(outputarr)
}

#test code
print(findNeighbors(testGrid1, c(2, 2)))
#should return:
#1 2
#2 3
#3 2
#2 1
print(findNeighbors(testGrid1, c(6, 3)))
#should return:
#6 2
#5 3
#6 4

###function to generate a random individual to be patient 0
generateRandomInfected = function(population){ 
  dim = dim(population)
  randRow = sample(1:dim[1], size = 1)
  randCol = sample(1:dim[2], size = 1)
  for(i in 1:1000){
    if(population[randRow, randCol] == 1){
      population[randRow, randCol] = 0
      break
    }
    else{
      randRow = sample(1:dim[1], size = 1)
      randCol = sample(1:dim[2], size = 1)
    }
  }
  
  return(population)
}


#if an unvaccinated individual is adjacent to an infected individual, the unvaccinated individual also gets infected
getsInfected <- function(popgrid, position){
  neighbors = findNeighbors(popgrid, position)
  flag = FALSE
  vec = c()
  for(i in 1:(length(neighbors)/2)){
    vec = c(vec, neighbors[i,])  
  }
  
  for(j in 1:length(vec)){
    if(j%%2 != 0){
      if(popgrid[vec[j], vec[j+1]] == 0 && popgrid[position[1], position[2]] != 2){
        return(TRUE)
      }
    }
  }
  return(flag)
  
}

# Test code
testGrid3 <- array(data = c(numeric(23) + 1, 0), dim = c(6, 4))
print(getsInfected(testGrid3, c(5, 4))) # Should print TRUE
print(getsInfected(testGrid3, c(1, 1))) # Should print FALSE


#calculate the fraction of infected individuals at the end of the simulation
fracInf = function(population){
  dimensions = dim(population)
  totalPop = dimensions[1]*dimensions[2]
  infected = 0
  for(i in 1:length(population)){
    if(population[i] == 0){
      infected = infected + 1
    }
  }
  return(infected/totalPop)
}


testGrid4 = array(data = (c(numeric(9), numeric(15) + 1)), dim = c(6, 4))
#test code 
#calculate the fraction of infected individuals (0s) in testGrid4 and store that fraction in fractionInfected.
fractionInfected = fracInf(testGrid4)

print(fractionInfected) # Should print 0.375



#run simulation with created functions
initialPop = createGrid(c(40,60), 0.1) #create population
initialPop = generateRandomInfected(initialPop) #assign one individual to be patient 0

infection = function(population){ 
  newPop = population
  u = dim(population)
  infCount = 0
  for(i in 1:u[1]){
    for(j in 1:u[2]){
      if(getsInfected(population, c(i, j)) == TRUE){
        newPop[i, j] = 0
      }
    }
  }
  return(newPop) #returns a population with new infections
}

#simulates the infection spread throughout the entire population
runInfection = function(population){
  dim = dim(initialPop)
  totalPop = dim[1]*dim[2]
  image(1:40, 1:60, initialPop)
  newPopulation = infection(initialPop)
  while(areIdentical(newPopulation, initialPop) == FALSE){
    image(newPopulation)
    Sys.sleep(0.1)
    initialPop = newPopulation
    newPopulation = infection(initialPop)
  }
  
  return(fracInf(newPopulation))
}

fractionInfected = runInfection(initialPop)



#test code
print(fractionInfected) #should be about 0.9





####Varying the vaccination rate from 2% to 98% in 2% increments
###Each time, store the final infected fraction and plot the infected fraction vs. vaccination rate.


runInfectionNoPlot = function(population){ ### runs the infection without plotting, to decrease run time
  dim = dim(population)
  totalPop = dim[1]*dim[2]
  newPopulation = infection(initialPop)
  while(areIdentical(newPopulation, initialPop) == FALSE){
    initialPop = newPopulation
    newPopulation = infection(initialPop)
  }
  
  return(fracInf(newPopulation))
}


vaccinationRate = seq(from = .02, to = .98, by = .02)
finalInfectFrac = c()

for(i in vaccinationRate){
  initialPop = createGrid(c(40,60), i)
  initialPop = generateRandomInfected(initialPop)
  finalInfectFrac = c(finalInfectFrac, runInfectionNoPlot(initialPop))
}

plot(finalInfectFrac, vaccinationRate, ylim=c(0,1), xlim=c(0,1), ylab='vaccination rate', xlab='final fraction of infected individuals', main = 'rate of infection vs vaccination')
















