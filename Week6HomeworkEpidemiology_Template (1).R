# BEFORE YOU TURN THIS IN, MAKE SURE TO CLEAR THE GLOBAL ENVIRONMENT AND TEST IT


############## Week 6 Homework ##############
############### Epidemiology ################

# Write your functions in the order of the handout.
# Use the test code provided to ensure that they work.
# Make sure you name each function as it is in the test code.
# DO NOT MODIFY THE PROVIDED TEST CODE. It will be used to grade your assignment.
# You should also try some test code of your own
# to make sure that the functions work for many cases.

############# Activity 1 ###################
# Function to set up the grid
print("Activity 1")

createGrid <- function(dimensions, fraction){
  pop = (dimensions[1]*dimensions[2])
  num2s = pop*fraction
  num1s = pop - num2s
  popvec = c()
  for(i in 1:num2s){
    popvec = c(popvec, 2)
  }
  for(i in 1:num1s){
    popvec = c(popvec, 1)
  }
  
  popvec1 = sample(popvec, size = pop)
  
  arr = array(data=popvec1, dim=dimensions)
  return(arr)
}

# Test code
# Should create a 6x4 grid with 18 2s and six 1s randomly distributed
print(createGrid(c(6,4), 0.75))


########### Activity 2 ########################
# Function to check whether two arrays are identical
print("Activity 2")

areIdentical <- function(arr1, arr2){
  flag = TRUE
  x = dim(arr1)
  y = dim(arr2)
  for(i in 1:length(x)){
    if(x[i] != y[i]){
      flag = FALSE
      break
    }
  }
  
  for(i in 1:length(arr1)){
    if(arr1[i] != arr2[i]){
      flag = FALSE
      break
    }
  }
  
  return(flag)
}

# Test code
testGrid1 <- array(data = c(numeric(23), 1), dim = c(6, 4))
testGrid2 <- testGrid1
print(areIdentical(testGrid1, testGrid2)) # Should print TRUE
testGrid2 <- array(data = c(numeric(10), 1, numeric(13)), dim = c(6, 4))
print(areIdentical(testGrid1, testGrid2)) # Should print FALSE
testGrid3 <- array(data=1:6, dim = c(3,2))
testGrid4 <- array(data=1:6, dim = c(2,3))
print(areIdentical(testGrid3, testGrid4)) # Should print FALSE
testGrid5 <- array(data=c(3,2,4,4,5,6), dim = c(2,3))
print(areIdentical(testGrid5, testGrid4)) # Should print FALSE



############# Activity 3 ######################
# Function to find the coordinates of all neighbors
# of a given spot in an array
print("Activity 3")

findNeighbors <- function(inputarr, position){
  dimensions = dim(inputarr)
  rownum = position[1]
  colnum = position[2]
  vec1 = c()
  vec2 = c()
  if(colnum != 1){
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
  
  vec3 = c(vec1, vec2)
  outputarr = array(data=vec3, dim=c((length(vec3)/2),2))
  return(outputarr)
}

# Test code
print(findNeighbors(testGrid1, c(2, 2)))
# Should print the following:
# 1 2
# 2 3
# 3 2
# 2 1
# Not necessarily in that order
print(findNeighbors(testGrid1, c(6, 3)))
# Should print the following:
# 6 2
# 5 3
# 6 4
# Not necessarily in that order


################ Activity 4 ################
# Put a 0 in a 40X60 grid that you make with createGrid().
# No test code for this one.
print("Activity 4")
generateRandomInfected = function(population){ ### function to generate a random individual to be patient 0
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

############## Activity 5 #################
# Function to decide whether an individual should be infected
print("Activity 5")

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


############### Activity 6 ################
# Code to calculate the fraction of infected individuals at the end of the simulation
# This doesn't have to be a function.
# Either way, store the fraction in a variable called fractionInfected.
print("Activity 6")
fracInf = function(population){
  dimensions = dim(population)
  totalPop = dimensions[1]*dimensions[2]
  nasty = 0
  for(i in 1:length(population)){
    if(population[i] == 0){
      nasty = nasty + 1
    }
  }
  return(nasty/totalPop)
}


testGrid4 <- array(data = (c(numeric(9), numeric(15) + 1)), dim = c(6, 4))
# Write code to calculate the fraction of infected individuals (0s) in testGrid4
# and store that fraction in fractionInfected.
fractionInfected = fracInf(testGrid4)


# Test code
print(fractionInfected) # Should print 0.375


############### Activity 7 #################
# Putting it all together
# Now write code to run the simulation once.
# Use the base model and 10% vaccination.
# Make sure you store fractionInfected at the end so that it prints to the console.
# To plot, look at activity 7b.  [For activity 8-14 you may want to comment it out]
print("Activity 7")

initialPop = createGrid(c(40,60), 0.1)
initialPop = generateRandomInfected(initialPop)

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
  return(newPop)
}

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



# Test code
print(fractionInfected) # Output may vary, but should be about 0.9




################ Activity 8 ###############
# Varying the vaccination rate
# Make the plot described in the handout for #8.
# Increase the vaccination rate from 2% to 98%, in increments of 2%.
# Each time, store the final infected fraction,
# and plot the infected fraction vs. vaccination rate.


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

plot(finalInfectFrac, vaccinationRate, ylim=c(0,1), xlim=c(0,1), ylab='vaccination rate', xlab='final fraction of infected individuals')




################ Activity 9 ###############
# Make the vaccine less than perfect
# Vary the vaccination rate
# Make a plot like in Activity 8.
# Increase the vaccination rate from 2% to 98%, in increments of 2%.
# Each time, store the final infected fraction,
# and plot the infected fraction vs. vaccination rate.





############### Activities 10-16 ##############
# Write any other code for making additional plots or modifying the model here.
# You need to do at least one of 10-16
# Comments or print statements describing what your code does are helpful!












