
# initialize how much genes should be in chromosome to represent player 1 and how much to represent player 2

player1ChromSize <- 2;
player2ChromSize <- 2;
chromSize <- player1ChromSize + player2ChromSize;

# initialize population size

populationSize <- 4;

# initialize possible actions for each player (they will be used to create chromosomes and perform mutations)

player1Actions <- c("A", "B", "G", "H");
player2Actions <- c("C", "D", "E", "F");

# create game and initialize population

game <- createGame();
population <- createPopulation();

# helper values : since each cell in game matrix contains utility functions for each of the players
# stored as "value for player 1, value for player 2", to speed up execution, those are separated to two matrices
# each of which corresponds to only one player

game1 = game;
game2 = game;
for(i in rownames(game)){
  for(j in colnames(game)){
    game1[i,j] = strsplit(game[i,j], ",")[[1]][1];
    game2[i,j] = strsplit(game[i,j], ",")[[1]][2];
  }
}

# example game where (0,0) encodes invalid action combinations

createGame <- function(){
  game = matrix("0,0", nrow = 6, ncol = 6);
  rownames(game) = c("AB", "AG", "AH", "BG", "BH", "GH");
  colnames(game) = c("CD", "CE", "CF", "DE", "DF", "EF");
  game["AG", "CE"] = game["AG", "CF"] = game["AH", "CE"] = game["AH", "CF"] = "4,9";
  game["BG", "CE"] = game["BG", "DE"] = game["BH", "CE"] = game["BH", "DE"] = "6,6";
  game["AG", "DE"] = game["AG", "DF"] = game["AH", "DE"] = game["AH", "DF"] = "9,4";
  game["BG", "CF"] = game["BG", "DF"] = "3,11";
  game["BH", "CF"] = game["BH", "DF"] = "2,1";
  return(game);
}

createPopulation <- function(){
  population = list();
  
  for(i in 1:populationSize){

    population[[i]] = vector(length = chromSize);

    # randomly choose actions for player 1 to fill the first part of the chromosome

    for(j in 1:player1ChromSize){

      tmpAction = player1Actions[sample(1:length(player1Actions), 1)];
      while(tmpAction %in% population[[i]]) tmpAction = player1Actions[sample(1:length(player1Actions), 1)];
      population[[i]][j] = tmpAction;
    
    }

    # element is sorted to preserve logical action flow

    population[[i]][1:player1ChromSize] = sort(population[[i]][1:player1ChromSize]);

    # randomly choose actions for player 2 to fill the second part of the chromosome
    
    for(j in (player1ChromSize+1):chromSize){
      
      tmpAction = player2Actions[sample(1:length(player2Actions), 1)];
      while(tmpAction %in% population[[i]]) tmpAction = player2Actions[sample(1:length(player2Actions), 1)];
      population[[i]][j] = tmpAction;    
    
    }

    # element is sorted to preserve logical action flow
    
    population[[i]][(player1ChromSize+1):chromSize] = sort(population[[i]][(player1ChromSize+1):chromSize]);
  }

  return(population);
}

# since game matrix is indexed with list of action names for a player (row player vs column player)
# it is necessary to extract column (row) name from element for each player to access the game matrix

# example: element = "ADE145" and player1ChromSize = 3 => to access row, use "ADE", and to access column use "145"

toColName <- function(element, player){
  res = "";
  if(player == 1){
    for(i in 1:player1ChromSize){
      res = paste(res, element[i], sep = "");
    }
  }else{
    for(i in (player1ChromSize+1):chromSize){
      res = paste(res, element[i], sep = "");
    }
  }
  return(res)
}

getFitness <- function(game, el){

  value = 0;
  
  accStr1 = toColName(el, 1);
  accStr2 = toColName(el, 2);
  
  if(!(accStr1 %in% rownames(game)) || !(accStr2 %in% colnames(game)))
    print(el);
  
  # player 1 action rank

  player1Options = sort(strtoi(unname(game1[,accStr2])));
  actionRank = which(player1Options == strsplit(game[accStr1,accStr2], ",")[[1]][1]);
  actionRank = unname(actionRank[length(actionRank)]);

  # cumulative fitness value with player 1

  value = value + actionRank * 100 / length(player1Options);
  
  # player 2 action rank 

  player2Options = sort(strtoi(unname(game2[accStr1,])));
  actionRank = which(player2Options == strsplit(game[accStr1,accStr2], ",")[[1]][2]);
  actionRank = unname(actionRank[length(actionRank)]);

  # cumulative finess value with both player 1 and 2

  value = value + actionRank * 100 / length(player2Options);

  # reset fitness value if action is invalid
  
  if(game1[accStr1, accStr2] == 0 && game2[accStr1, accStr2] == 0){
    value = 0;
  }
  
  return(value);
}

selection <- function(game, population){
  
  # roulette wheel selection
  
  tmp = orderPopulation(game, population, FALSE);

  totalSum = Reduce("+", sapply(tmp, "[", 1));
  r = sample(1:totalSum, 1);
  currentSum = tmp[[1]][[1]];
  i = 1;
  
  while(currentSum < r && i < length(tmp)){
    i = i + 1;
    currentSum = currentSum + tmp[[i]][[1]];
  }
  
  tmp = sapply(tmp, "[", 2);
  
  return(unlist(tmp[[i]]));
}

reproduce <- function(el1, el2){
  
  res = vector(length = chromSize);
  
  for(i in 1:chromSize){
    res[i] = "z";
  }

  # pick a random value to indicate at what gene to split up the chromosome
  
  x = sample(1:(chromSize-1), 1);
  
  for(i in 1:x){
      
      # copy action value (gene) from 1st parent element

      action = el1[i];

      # if there is already that action in gene, replace it with randomly chosen action

      while(action %in% res){
        if(x < chromSize/2){
          action = player1Actions[sample(1:length(player1Actions), 1)];
        }else{
          action = player2Actions[sample(1:length(player2Actions), 1)];
        }
      }
      res[i] = action;   
  }

  # repeat for 2nd parent element
  
  for(i in (x+1):chromSize){
    action = el2[i];
    while(action %in% res){
      if(x < chromSize/2){
        action = player1Actions[sample(1:length(player1Actions), 1)];
      }else{
        action = player2Actions[sample(1:length(player2Actions), 1)];
      }
    }
    res[i] = action; 
  }
  
  # sort resulting chromosome to maintain the logical action flow
  
  res[1:(chromSize/2)] = sort(res[1:(chromSize/2)]);
  res[(chromSize/2+1):chromSize] = sort(res[(chromSize/2+1):chromSize]);
    
  return(res);
}

mutate <- function(el){

	# pick first or second player's part in the chromosome at random

  if(sample(1:2, 1) == 1){

  	# pick gene at random in the appropriate part of the chromosome
    
    index = sample(1:player1ChromSize, 1);

    # pick a random action from the appropriate player

    action = player1Actions[sample(1:length(player1Actions), 1)];

    # if the chosen action already exists in the chromosome, pick another at random while it does not exist

    while(action %in% el) action = player1Actions[sample(1:length(player1Actions), 1)];
    el[index] = action;

  }else{
    
    index = sample((player1ChromSize+1):populationSize, 1);
    action = player2Actions[sample(1:length(player2Actions), 1)];
    while(action %in% el) action = player2Actions[sample(1:length(player2Actions), 1)];
    el[index] = action;
  
  }

  # sort resulting chromosome part by part
  
  el[(player1ChromSize+1):populationSize] = sort(el[(player1ChromSize+1):populationSize]);
  el[1:player1ChromSize] = sort(el[1:player1ChromSize]);
  
  return(el);
}

# sort population based on fitness value

orderPopulation <- function(game, population, cleared = TRUE){
  tmp = list();
  
  for(i in 1:length(population)){
    tmp[[i]] <- c(getFitness(game, population[[i]]), population[i]);
  }
  
  tmp = tmp[order(sapply(tmp, "[[", 1), na.last = TRUE, decreasing = TRUE)];
  
  if(cleared){
    return(sapply(tmp, "[", 2));
  }else{
    return(tmp);
  }
}

geneticAlgorithm <- function(game, population, stepLimit){
  
  step = 0;
  solution = NULL;
  
  # 100% best response from each player
  maxFitness = 200;
  
  while(is.null(solution) && step < stepLimit){
    print("POPULATION");
    print(population);
    newPopulation = list();

    # generate new population

    for(i in 1:length(population)){
      parent1 = selection(game, population);
      parent2 = selection(game, population);
      child = reproduce(parent1, parent2);
      if(sample(1:100, 1) > 90) child = mutate(child);
      newPopulation[[i]] = child;

      # check if equilibrium is achieved

      if(getFitness(game,child) == maxFitness)
        solution = child;
    }
    population = newPopulation;
    step = step+1;
  }
  
  if(is.null(solution)){

  	# if equilibrium is not achieved, return the chromosome with best fitness value from the current population

    solution = orderPopulation(game, population)[[1]];
    
  }else{
    print("Equilibrium achieved");
  }
  
  return(solution);
  
}
