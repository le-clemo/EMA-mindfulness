#Permutation Test for Stability Analysis and Network Comparison


extract_cent <- function(cent.table, n, m){
  #function to get a specific centrality measure for a specific node from a centrality table
  if(m %in% unique(cent.table$measure)){
    cent.val <- cent.table[which((cent.table$node==n) & cent.table$measure==m),]$value
    
  } else {cent.val <- 0}
  
  return(cent.val)
}


NPT <- function(data, nodes, filepath, permuteBy="nodes", iterations=100,
                idvar="subjB", dayvar="phaseAssessmentDay", beepvar="dayBeepNum",
                localClustStrength=list(),
                nCores=detectCores()-2){
  
  #if permuteBy = "nodes" this function checks a networks ~robustness by shuffling the nodes per subject (i.e., the columns of the provided data)
  #at each iteration
  #else one has to provide the column name of the variable to be permuted by (e.g., "group", or "phase").
  #Note that the latter case will lead to a comparison test between two networks (e.g. group1 vs group2)
  
  if(missing(filepath)) stop("filepath required --> filepath.rda")
  
  testSummary <- list()
  testStats <- list()
  edgeWeights <- list()
  globalStrengths <- list()
  
  if(length(localClustStrength)>0){
    localStrengths <- list()
    n_local <- length(localClustStrength)
    for(l in 1:n_local){
      localStrengths[[l]] <- list()
    }
  }
  
  start.time <- Sys.time()
  
  #number of permutations
  perms <- iterations
  nodeVars <- nodes
  n_nodeVars <- length(nodeVars)
  
  #centrality measures for contemporaneous and temporal networks, respectively
  measures.cont <- c("Strength", "ExpectedInfluence")
  measures.temp <- c("InStrength", "OutStrength", "InExpectedInfluence", "OutExpectedInfluence")
  measures <- c(measures.cont, measures.temp)
  
  if(permuteBy=="nodes"){
    permuteSet <- "network"
    n_idVars <- length(c(idvar, dayvar, beepvar))
    
    #actual data
    real.dat <- data[,c(idvar, dayvar, beepvar, nodeVars)]
    
  } else {
    permuteSet <- unique(data[[permuteBy]])
    n_idVars <- length(c(idvar, dayvar, beepvar, permuteBy))
    
    #actual data
    real.dat <- data[,c(idvar, dayvar, beepvar, nodeVars, permuteBy)]
  }
  
  
  if(file.exists(filepath)){
    
    f <- load(filepath)
    
    prev_results <- mget(f)[["permutationResults"]]
    
    #number of permutations saved in file (??minus 1 because it also contains to "true" file)
    
    #add stats of real net(s) to testSummary
    for(list_item in names(prev_results)){
      if(list_item == "p_values"){
        break
      }
      testSummary[[list_item]] <- prev_results[[list_item]]
    }
    
    prev_iter <- length(prev_results$testStats[[permuteSet[1]]]$EdgeWeights$Temporal) #- 1
    
    cent.df <- data.frame(matrix(ncol = length(measures), nrow = prev_iter + perms))
    colnames(cent.df) <- measures
    
    print(paste("# previous iterations:", prev_iter, sep = " "))
    print(paste("# permutations to perform:", perms, sep = " "))
    
    for(g in permuteSet){
      testStats[[g]] <- list(Centrality = list(), EdgeWeights = list(Temporal = list(), Contemporaneous = list()),
                             GlobalStrength = list(Temporal = list(), Contemporaneous = list()))
      
      # print("testStats[[g]] created")
      
      for(n in nodeVars){
        testStats[[g]]$Centrality[[n]] <- cent.df
        
        testStats[[g]]$Centrality[[n]][1:(prev_iter),] <- prev_results$testStats[[g]]$Centrality[[n]]
        
        # print(testStats[[g]]$Centrality[[n]])
      }
      
      edgeWeights[[g]] <- list(Temporal = prev_results$testStats[[g]]$EdgeWeights$Temporal,
                               Contemporaneous = prev_results$testStats[[g]]$EdgeWeights$Contemporaneous)
      
      globalStrengths[[g]] <- list(Temporal = prev_results$testStats[[g]]$GlobalStrength$Temporal,
                                   Contemporaneous = prev_results$testStats[[g]]$GlobalStrength$Contemporaneous)
      
      if(length(localClustStrength)>0){
        for(l in 1:length(prev_results$testStats[[g]]$LocalStrength)){
          localStrengths[[l]][[g]] <- list(Temporal = prev_results$testStats[[g]]$LocalStrength[[l]]$Temporal,
                                           Contemporaneous = prev_results$testStats[[g]]$LocalStrength[[l]]$Contemporaneous)
        }
      }
    }
    
    #and a list for edge weight matrices per permutation
    
    
    #create vectors with appropriate size and populate first prev_iter positions
    # gs.cont.vec <- c(rep(NA, (prev_iter + perms)))
    # gs.temp.vec <- c(rep(NA, (prev_iter + perms)))
    # gs.cont.vec[1:(prev_iter)] <- f$testStats$GlobalStrength$Contemporaneous
    # gs.temp.vec[1:(prev_iter)] <- f$testStats$GlobalStrength$Temporal
    
  } else {
    
    testSummary$data <- real.dat
    
    prev_iter = 1 #the real network is the first and only previous iteration
    
    # #create a named nested list with centrality measures per node
    # testStats <- list()
    # #and a list for edge weight matrices per permutation
    # edgeWeights <- list()
    # globalStrengths <- list() #Contemporaneous = list(), Temporal = list()
    
    #create empty dataframes for with row = permutation number and col = centrality measures
    cent.df <- data.frame(matrix(ncol = length(measures), nrow = perms+1))
    colnames(cent.df) <- measures
    
    #also for real nets only with row = nodes and col = centrality measure
    real.cont.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.cont)))
    colnames(real.cont.df) <- measures.cont
    rownames(real.cont.df) <- nodeVars
    
    real.temp.df <- data.frame(matrix(nrow = length(nodeVars), ncol = length(measures.temp)))
    colnames(real.temp.df) <- measures.temp
    rownames(real.temp.df) <- nodeVars
    
    # gs.cont.vec <- c(rep(NA, (perms + 1)))
    # gs.temp.vec <- c(rep(NA, (perms + 1)))
    
    for(g in permuteSet){
      print(paste('----', g, '----', sep = ' '))
      testSummary[[g]] <- list(Temporal = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()),
                               Contemporaneous = list(Centrality = list(), EdgeWeights = list(), GlobalStrength = list()))
      
      testStats[[g]] <- list(Centrality = list(), EdgeWeights = list(Temporal = list(), Contemporaneous = list()),
                             GlobalStrength = list(Temporal = list(), Contemporaneous = list()))
      
      
      edgeWeights[[g]] <- list()
      
      globalStrengths[[g]] <- list()
      
      if(length(localClustStrength)>0){
        for(l in 1:length(localClustStrength)){
          localStrengths[[l]][[g]] <- list()
        }
      }
      
      
      if(permuteSet[1] != "network"){
        real.dat <- data[which(data[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]
      }
      
      real.net <- mlVAR(real.dat,
                        vars=nodeVars,
                        estimator="lmer",
                        idvar=idvar,
                        dayvar=dayvar,
                        beepvar=beepvar,
                        lags = 1,
                        temporal = "orthogonal",
                        contemporaneous = "orthogonal",
                        nCores = nCores)
      
      #  # Get mlVAR networks:
      real.cont <- getNet(real.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      real.temp <- getNet(real.net, "temporal", nonsig = "hide")
      
      # 
      # n1 <- qgraph(real.temp, layout = L, theme='colorblind', negDashed=FALSE, diag=T, #title=paste("Controls: Temporal - Baseline")
      #              groups=groups_list, legend=FALSE, nodeNames = nodeVars, labels=c(1:length(nodeVars)),
      #              vsize=10, asize=8, curve=0.5, esize=3)
      # 
      # print(n1)
      
      #get all centrality measures
      real.cont.cents <- centralityTable(real.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      real.temp.cents <- centralityTable(real.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
      print("++++++++++++++++++++++++ Net created ++++++++++++++++++++++++")
      
      #add real centralities
      for(n in nodeVars){
        testStats[[g]]$Centrality[[n]] <- cent.df
        for(m in measures.cont){
          val <- extract_cent(real.cont.cents, n, m)
          testStats[[g]]$Centrality[[n]][1,m] <- val
          real.cont.df[n,m] <- val
          
        }
        for(m in measures.temp){
          val <- extract_cent(real.temp.cents, n, m)
          testStats[[g]]$Centrality[[n]][1,m] <- val
          real.temp.df[n,m] <- val
        }
        
        #print(cent.df)
        # testStats[[g]]$Centrality[[n]] <- cent.df
      }
      
      print("Centralities added")
      
      #add edge weight matrices
      edgeWeights[[g]]$Contemporaneous[[1]] <- real.cont
      edgeWeights[[g]]$Temporal[[1]] <- real.temp
      
      #global strength
      gs.cont <- sum(abs(real.cont[which(upper.tri(real.cont))]))
      gs.temp <- sum(abs(real.temp[which(upper.tri(real.temp))]))
      
      globalStrengths[[g]]$Contemporaneous[[1]] <- gs.cont
      globalStrengths[[g]]$Temporal[[1]] <- gs.temp
      
      
      testSummary[[g]]$Contemporaneous$Centrality <- real.cont.df
      testSummary[[g]]$Temporal$Centrality <- real.temp.df
      testSummary[[g]]$Contemporaneous$EdgeWeights <- real.cont
      testSummary[[g]]$Temporal$EdgeWeights <- real.temp
      testSummary[[g]]$Contemporaneous$GlobalStrength <- gs.cont
      testSummary[[g]]$Temporal$GlobalStrength <- gs.temp
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
          clust <- localClustStrength[[l]]
          contSum <- 0
          tempSum <- 0
          for(cl in clust){
            contSum <- contSum + sum(real.cont[cl, clust]) #clust[which(clust %nin% cl)]])
            tempSum <- tempSum + sum(real.temp[cl, clust]) #[which(clust %nin% cl)]])
          }
          localStrengths[[l]][[g]]$Contemporaneous[[1]] <- contSum
          localStrengths[[l]][[g]]$Temporal[[1]] <- tempSum
          
          testSummary[[g]]$Contemporaneous$LocalStrenghts[[l]] <- contSum
          testSummary[[g]]$Temporal$LocalStrenghts[[l]] <- tempSum
          
        }
      }
    }
    
  }
  
  #permutation test statistics
  s_ids <- unique(real.dat[[idvar]])
  print("Number of subjects:")
  print(length(s_ids))
  
  
  for(i in (prev_iter+1):(prev_iter+perms)){
    print(paste("Permutation", i-1, sep = " "))
    
    if(permuteSet == "network"){
      #create permuted data set (shuffled node labels per subject)
      
      perm.dat <- data.frame(matrix(ncol = length(c(idvar, dayvar, beepvar, nodeVars))))
      colnames(perm.dat) <- c(idvar, dayvar, beepvar, nodeVars)
      
      for(s in s_ids){
        #subset for only one subject 
        s_df <- real.dat[which(real.dat[[idvar]]==s),]
        
        #shuffle node lables for subject
        perm.vars <- sample(nodeVars)
        colnames(s_df)[(n_idVars + 1) : (n_nodeVars + n_idVars)] <- perm.vars
        
        #add to permutation data set
        perm.dat <- rbind(perm.dat, s_df)
        pg.dat <- copy(perm.dat)
        
      }
      
    } else {
      
      perm.dat <- data.frame(matrix(ncol = length(c(idvar, dayvar, beepvar, permuteBy, nodeVars))))
      colnames(perm.dat) <- c(idvar, dayvar, beepvar, permuteBy, nodeVars)
      
      for(s in s_ids){
        # print(s)
        #subset for only one subject 
        s_df <- data[which(data[[idvar]]==s),c(idvar, dayvar, beepvar, permuteBy, nodeVars)]
        
        #shuffle node lables for subject
        perm.group <- sample(permuteSet, 1)
        
        s_df[[permuteBy]] <- perm.group
        
        #add to permutation data set
        perm.dat <- rbind(perm.dat, s_df)
        
      }
      
      #each group needs to have at least two subjects for mlVAR to run
      n_g1 <- length(unique(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]]))
      n_g2 <- length(unique(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[2]),][[idvar]]))
      
      print(n_g1)
      print(n_g2)
      if(n_g1 < 2){
        print("Too few g1 samples")
        if(n_g1 == 0){
          n <- 2
        } else if(n_g1 == 1){
          n <- 1
        }
        rand.subj <- sample(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[2]),][[idvar]], n)
        perm.dat[which(perm.dat[[idvar]] %in% rand.subj),][[permuteBy]] <- permuteSet[1]
      }  
      
      if(n_g2 < 2){
        print("Too few g2 samples")
        if(n_g2 == 0){
          n <- 2
        } else if(n_g2 == 1){
          n <- 1
        }
        rand.subj <- sample(perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]], n)
        perm.dat[which(perm.dat[[idvar]] %in% rand.subj),][[permuteBy]] <- permuteSet[2]
      }
      
      
      s <- perm.dat[which(perm.dat[[permuteBy]]==permuteSet[1]),][[idvar]]
      
      #in case only one group was assigned to all subjects
      sampled.groups <- unique(perm.dat[[permuteBy]])
      sampled.groups <- sampled.groups[!is.na(sampled.groups)]
      # print("This is the sampled group:")
      # print(sampled.groups)
      # print(length(sampled.groups))
      if(length(sampled.groups) < 2){ #to ensure we end up with two groups
        print("Fixing issue")
        y <- permuteSet[which(permuteSet != sampled.groups)]
        print(y)
        random.subj <- sample(s_ids, 1)
        print(random.subj)
        perm.dat[which(perm.dat[[idvar]] == random.subj),][[permuteBy]] <- y
        print(unique(perm.dat[[permuteBy]]))
      }
    }
    
    permuteSet <- permuteSet[!is.na(permuteSet)]
    
    for(g in permuteSet){
      print(g)
      
      if(permuteSet[1] != "network"){
        pg.dat <- perm.dat[which(perm.dat[[permuteBy]]==g),c(idvar, permuteBy, dayvar, beepvar, permuteBy, nodeVars)]
        pg.dat[[idvar]] <- as.factor(pg.dat[[idvar]])
        # print(unique(pg.dat[[idvar]]))
        # print(levels(pg.dat[[idvar]]))
      }
      
      #fit permutated network
      perm.net <- mlVAR(pg.dat,
                        vars=nodeVars,
                        estimator="lmer",
                        idvar=idvar,
                        dayvar=dayvar,
                        beepvar=beepvar,
                        lags = 1,
                        temporal = "orthogonal",
                        contemporaneous = "orthogonal",
                        nCores = nCores)
      
      # print("Net created")
      
      #  # Get mlVAR networks:
      perm.cont <- getNet(perm.net, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
      perm.temp <- getNet(perm.net, "temporal", nonsig = "hide")
      
      # print("Nets extracted")
      
      edgeWeights[[g]]$Contemporaneous[[i]] <- perm.cont
      edgeWeights[[g]]$Temporal[[i]] <- perm.temp
      
      # print("Edge weights added")
      
      #global strength
      perm.gs.cont <- sum(abs(perm.cont[which(upper.tri(perm.cont))]))
      perm.gs.temp <- sum(abs(perm.temp[which(upper.tri(perm.temp))]))
      globalStrengths[[g]]$Contemporaneous[[i]] <- perm.gs.cont
      globalStrengths[[g]]$Temporal[[i]] <- perm.gs.temp
      
      # print("Global Strength added")
      
      #get permuted centrality measures
      perm.cont.cents <- centralityTable(perm.cont, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      perm.temp.cents <- centralityTable(perm.temp, weighted = TRUE, labels = nodeVars, standardized = FALSE)
      
      # print("centrality tables created")
      
      #add permuted centralities
      for(n in nodeVars){
        # print(n)
        for(m in measures.cont){
          # print(m)
          val <- extract_cent(perm.cont.cents, n, m)
          testStats[[g]]$Centrality[[n]][i,m] <- val
          
        }
        for(m in measures.temp){
          # print(m)
          val <- extract_cent(perm.temp.cents, n, m)
          testStats[[g]]$Centrality[[n]][i,m] <- val
          
        }
      }
      
      # print("Centrality added to testStats")
      
      testStats[[g]]$EdgeWeights <- list(Temporal = edgeWeights[[g]]$Temporal,
                                         Contemporaneous = edgeWeights[[g]]$Contemporaneous)
      testStats[[g]]$GlobalStrength <- list(Temporal = globalStrengths[[g]]$Temporal,
                                            Contemporaneous = globalStrengths[[g]]$Contemporaneous)
      
      # print("EdgeWeigthts added to testStats")
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
          clust <- localClustStrength[[l]]
          contSum <- 0
          tempSum <- 0
          for(cl in clust){
            contSum <- contSum + sum(perm.cont[cl, clust]) #[which(clust %nin% cl)]])
            tempSum <- tempSum + sum(perm.temp[cl, clust]) #[which(clust %nin% cl)]])
          }
          localStrengths[[l]][[g]]$Contemporaneous[[i]] <- contSum
          localStrengths[[l]][[g]]$Temporal[[i]] <- tempSum
          
          testStats[[g]]$LocalStrengths[[l]] <- list(Temporal = localStrengths[[l]][[g]]$Temporal,
                                                     Contemporaneous = localStrengths[[l]][[g]]$Contemporaneous)
        }
      }
      
    }
    # print(testStats$controls$GlobalStrength)
    
  }
  
  # print('TestStats (controls - ruminating):')
  # print(testStats$controls$Centrality$ruminating)
  
  if(permuteSet[1] != "network"){
    
    print("Calculating difference scores")
    
    diff.df <- data.frame(matrix(ncol = length(measures), nrow = (prev_iter + perms)))
    colnames(diff.df) <- measures
    
    testStats$difference <- list()
    
    p_valBase <- "difference"
    for(n in nodeVars){
      testStats$difference$Centrality[[n]] <- diff.df
      # print('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
      # print(paste("Iteration", i, sep = " "))
      for(i in 1:(prev_iter+perms)){
        # print(n)
        # print(testStats$difference[[n]])
        # print('----------------------------------------------------------------------------')
        # print(testStats[[permuteSet[2]]]$Centrality[[n]][i,])
        # print(testStats[[permuteSet[1]]]$Centrality[[n]][i,])
        
        testStats$difference$Centrality[[n]][i,] <- testStats[[permuteSet[2]]]$Centrality[[n]][i,] - testStats[[permuteSet[1]]]$Centrality[[n]][i,]
        
        # print(testStats$difference[[n]][i,])
      }
      # print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
      # print(testStats$difference)
    }
    # print("Centralities added")
    for(i in 1:(prev_iter+perms)){
      
      edgeWeights$difference$Contemporaneous[[i]] <- edgeWeights[[permuteSet[2]]]$Contemporaneous[[i]] -
        edgeWeights[[permuteSet[1]]]$Contemporaneous[[i]]
      
      edgeWeights$difference$Temporal[[i]] <- edgeWeights[[permuteSet[2]]]$Temporal[[i]] -
        edgeWeights[[permuteSet[1]]]$Temporal[[i]]
      
      
      globalStrengths$difference$Contemporaneous[[i]] <- globalStrengths[[permuteSet[2]]]$Contemporaneous[[i]] -
        globalStrengths[[permuteSet[1]]]$Contemporaneous[[i]]
      
      globalStrengths$difference$Temporal[[i]] <- globalStrengths[[permuteSet[2]]]$Temporal[[i]] -
        globalStrengths[[permuteSet[1]]]$Temporal[[i]]
      
      #local strengths
      if(length(localClustStrength)>0){
        for(l in 1:n_local){
          localStrengths[[l]]$difference$Contemporaneous[[i]] <- localStrengths[[l]][[permuteSet[2]]]$Contemporaneous[[i]] -
            localStrengths[[l]][[permuteSet[1]]]$Contemporaneous[[i]]
          
          localStrengths[[l]]$difference$Temporal[[i]] <- localStrengths[[l]][[permuteSet[2]]]$Temporal[[i]] -
            localStrengths[[l]][[permuteSet[1]]]$Temporal[[i]]
          
        }
      }
      
    }
    
    
    # print(edgeWeights)
    # print(globalStrengths)
    
  } else { p_valBase <- "network"}
  
  
  testStats[[p_valBase]]$EdgeWeights <- list(Temporal = edgeWeights[[p_valBase]]$Temporal,
                                             Contemporaneous = edgeWeights[[p_valBase]]$Contemporaneous)
  testStats[[p_valBase]]$GlobalStrength <- list(Temporal = globalStrengths[[p_valBase]]$Temporal,
                                                Contemporaneous = globalStrengths[[p_valBase]]$Contemporaneous)
  
  #local strengths
  if(length(localClustStrength)>0){
    for(l in 1:n_local){
      testStats[[p_valBase]]$LocalStrengths[[l]] <- list(Temporal = localStrengths[[l]][[p_valBase]]$Temporal,
                                                         Contemporaneous = localStrengths[[l]][[p_valBase]]$Contemporaneous)
    }
  }
  
  #dataframe to store centrality measure p-vals
  c.cent.df <- data.frame(matrix(ncol = length(measures.cont), nrow = length(nodeVars)))
  colnames(c.cent.df) <- measures.cont
  rownames(c.cent.df) <- nodeVars
  
  t.cent.df <- data.frame(matrix(ncol = length(measures.temp), nrow = length(nodeVars)))
  colnames(t.cent.df) <- measures.temp
  rownames(t.cent.df) <- nodeVars
  
  #dataframe for edge weight p-vals for contemporaneous and temporal
  c.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
  colnames(c.ew.df) <- nodeVars
  rownames(c.ew.df) <- nodeVars
  
  t.ew.df <- data.frame(matrix(ncol = length(nodeVars), nrow = length(nodeVars)))
  colnames(t.ew.df) <- nodeVars
  rownames(t.ew.df) <- nodeVars
  
  excluded.edges <- c(rep(NA, length(nodeVars)))
  
  #print(testStats)
  # print(p_valBase)
  
  for(n in nodeVars){
    # print(paste("++++++++++++++++++", n, "++++++++++++++++++", sep = " "))
    # print(testStats[[p_valBase]]$Centrality[[n]])
    for(m in measures.cont){
      # print(paste("###", m, "###", sep = " "))
      # print(testStats[[p_valBase]]$Centrality[[n]][m])
      
      # pval <- mean(abs((testStats[[p_valBase]]$Centrality[[n]][m])) >= (abs(testStats[[p_valBase]]$Centrality[[n]][1,m])))
      
      if(testStats[[p_valBase]]$Centrality[[n]][1,m] > 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) >= (testStats[[p_valBase]]$Centrality[[n]][1,m]))
        
      } else if(testStats[[p_valBase]]$Centrality[[n]][1,m] < 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) <= (testStats[[p_valBase]]$Centrality[[n]][1,m]))
        
      } else {
        pval <- 1
      }
      
      c.cent.df[n,m] <- pval
      # print(pval)
    }
    
    # print("Cont. cent pvals calculated")
    
    for(m in measures.temp){
      
      # pval <- mean(abs((testStats[[p_valBase]]$Centrality[[n]][m])) >= (abs(testStats[[p_valBase]]$Centrality[[n]][1,m])))
      # print(paste("###", m, "###", sep = " "))
      # print(testStats$Centrality[[n]][m])
      if(testStats[[p_valBase]]$Centrality[[n]][1,m] > 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) >= (testStats[[p_valBase]]$Centrality[[n]][1,m]))
        
      } else if(testStats[[p_valBase]]$Centrality[[n]][1,m] < 0){
        pval <- mean((testStats[[p_valBase]]$Centrality[[n]][m]) <= (testStats[[p_valBase]]$Centrality[[n]][1,m]))
        
      } else {
        pval <- 1
      }
      
      t.cent.df[n,m] <- pval
      # print(pval)
    }
  }
  
  # print("Temp cent pvals calculated")
  
  # print(edgeWeights[[p_valBase]]$Contemporaneous)
  
  j <- 1
  for(n in nodeVars){
    
    #now we add the current node to the excluded edges because in contemporaneous nets we do not have self-loops
    excluded.edges[j] <- n
    
    cont.weights <- c(rep(NA, prev_iter + perms))
    cont.edges <- nodeVars[nodeVars %nin% excluded.edges]
    
    j <- j + 1
    
    for(e in cont.edges){
      # print("++++++++++++++++++ Contemporaneous ++++++++++++++++++")
      # print(paste(n, "--", e, sep = " "))
      # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
      for(i in 1:(prev_iter + perms)){
        cont.weights[i] <- edgeWeights[[p_valBase]]$Contemporaneous[[i]][n,e]
      }
      
      # pval <- mean(abs((cont.weights)) >= (abs(cont.weights[1])))
      
      if(cont.weights[1] > 0){
        pval <- mean((cont.weights) >= (cont.weights[1]))
        
      } else if(cont.weights[1] < 0){
        
        pval <- mean((cont.weights) <= (cont.weights[1]))
        
      } else {
        pval <- 1
      }
      # print(pval)
      c.ew.df[n,e] <- pval
    } 
    
    #in temporal networks we have directed edges
    temp.weights <- c(rep(NA, prev_iter + perms))
    temp.edges <- nodeVars
    
    for(e in temp.edges){
      # print("++++++++++++++++++ Temporal ++++++++++++++++++")
      # print(paste(n, "-->", e, sep = " "))
      # print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
      for(i in 1:(prev_iter + perms)){
        temp.weights[i] <- edgeWeights[[p_valBase]]$Temporal[[i]][n,e]
      }
      
      # pval <- mean(abs((temp.weights)) >= (abs(temp.weights[1])))
      
      if(temp.weights[1] > 0){
        pval <- mean((temp.weights) >= (temp.weights[1]))
        
      } else if(temp.weights[1] < 0){
        pval <- mean((temp.weights) <= (temp.weights[1]))
        
      } else {
        pval <- 1
      }
      # print(pval)
      t.ew.df[n,e] <- pval
    }
    
  }
  # print("Ew pvalues added")
  
  gs.cont.vec <- c(rep(NA, prev_iter + perms))
  gs.temp.vec <- c(rep(NA, prev_iter + perms))
  for(i in 1:(prev_iter + perms)){
    gs.cont.vec[i] <- globalStrengths[[p_valBase]]$Contemporaneous[[i]]
    gs.temp.vec[i] <- globalStrengths[[p_valBase]]$Temporal[[i]]
  }
  
  gs.cont.pval <- mean((abs(gs.cont.vec)) >= ((abs(gs.cont.vec[1]))))
  gs.temp.pval <- mean((abs(gs.temp.vec)) >= ((abs(gs.temp.vec[1]))))
  
  #local strengths
  if(length(localClustStrength)>0){
    ls.cont.vec <- list()
    ls.temp.vec <- list()
    
    for(l in 1:n_local){
      ls.cont.vec[[l]] <- c(rep(NA, prev_iter + perms))
      ls.temp.vec[[l]] <- c(rep(NA, prev_iter + perms))
      for(i in 1:(prev_iter + perms)){
        ls.cont.vec[[l]][[i]] <- localStrengths[[l]][[p_valBase]]$Contemporaneous[[i]]
        ls.temp.vec[[l]][[i]] <- localStrengths[[l]][[p_valBase]]$Temporal[[i]]
      }
      
      ls.cont.pval <- mean((abs(ls.cont.vec[[l]])) >= ((abs(ls.cont.vec[[l]][[1]]))))
      ls.temp.pval <- mean((abs(ls.temp.vec[[l]])) >= ((abs(ls.temp.vec[[l]][[1]]))))
      
      testSummary$p_values$Contemporaneous$LocalStrengths[[l]] <- ls.cont.pval
      testSummary$p_values$Temporal$LocalStrengths[[l]] <- ls.temp.pval
    }
  }
  
  # print("Global Strength pvalues added")
  
  testSummary$p_values$Contemporaneous$Centrality <- c.cent.df
  testSummary$p_values$Temporal$Centrality <- t.cent.df
  testSummary$p_values$Contemporaneous$EdgeWeights <- c.ew.df
  testSummary$p_values$Temporal$EdgeWeights <- t.ew.df
  testSummary$p_values$Contemporaneous$GlobalStrength <- gs.cont.pval
  testSummary$p_values$Temporal$GlobalStrength <- gs.temp.pval
  
  
  # testStats[[p_valBase]]$EdgeWeights <- list(Temporal = edgeWeights[[p_valBase]]$Temporal,
  #                                    Contemporaneous = edgeWeights[[p_valBase]]$Contemporaneous)
  # testStats[[p_valBase]]$GlobalStrength <- list(Temporal = globalStrengths[[p_valBase]]$Temporal,
  #                                       Contemporaneous = globalStrengths[[p_valBase]]$Contemporaneous)
  
  testSummary$testStats <- testStats
  
  permutationResults <- copy(testSummary)
  
  save(permutationResults, file = filepath)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Time elapsed:", round(time.taken, 2), sep = " "))
  
  return(permutationResults)
}




