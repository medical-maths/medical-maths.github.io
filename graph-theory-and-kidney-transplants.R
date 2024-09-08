# Graph Theory and Kidney Transplants

# 1. Creating an Example Adjacency Matrix
S_adj_mat <- matrix(c(# Let's walk through how we fill out the first row of the adjacency matrix:
  # Row 1 represents the people Student A can sit with; 
  # they form a compatible pair with Students D, E, and F,
  # so there is a 1 in column 4, representing Student D,
  # a 1 in column 5, representing Student E,
  # and a 1 in column 6, representing Student F. 
  # The pairings A-B and A-C are unacceptable so there are 0s in columns 2 and 3. 
  # Vertex A is not connected to itself, because Student A cannot form a pair with themselves,
  # so there is a 0 in column 1 – in fact, all the diagonal values of this adjacency matrix are 0
  # because none of the vertices have edges connecting them to themselves.
  0, 0, 0, 1, 1, 1, 
  0, 0, 1, 0, 1, 0,
  0, 1, 0, 1, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0), 
  nrow = 6, byrow = TRUE)
print(S_adj_mat)

# 2. Converting an Adjacency Matrix to an Adjacency List
# Let's write a function which takes as its input a graph's adjacency matrix, and returns the adjacency list for the graph 
adj_mat_to_adj_list <- function(adj_mat){
  num_nodes <- ncol(adj_mat) # we find the number of vertices of the graph
  adjacency_list <- vector(mode = "list", length = num_nodes) # we initialise our adjacency list as an empty list with the same number of elements of vertices of the graph
  # Now we make an empty list for each vertex and fill it with the indexes of the vertices that it is connected to:
  for (i in 1:num_nodes){
    connected_vertices <- list()
    for (j in 1:num_nodes) {
      if (adj_mat[i,j] == 1) {
        connected_vertices <- c(connected_vertices, j)
      }
    }
    adjacency_list[[i]] <- connected_vertices
  }
  adjacency_list
}
# We can use this function to find the adjacency list for S:
S_adj_list <- adj_mat_to_adj_list(S_adj_mat)
names(S_adj_list) <- c("A","B","C","D","E","F")
# And find all the vertices that are connected to vertex A, for example:
vert_A_connections <- S_adj_list$A
LETTERS[unlist(vert_A_connections)]

# 3. Converting an Adjacency Matrix to an Edge List
# We can write a simple function to get the edge list matrix of a graph from it's adjacency matrix:
adj_mat_to_edge_list <- function(adj_mat) {
  edge_list <- unique(t(apply(which(adj_mat == 1, arr.ind = TRUE), 1, sort)))
  edge_list
}
# Let's see what the output looks like for S:
S_edge_list <- adj_mat_to_edge_list(S_adj_mat)
print(adj_mat_to_edge_list(S_adj_mat))
# We can convert the vertex numbers to letters to check we've got the result we've expected:
apply(S_edge_list, 1:2, function(i) LETTERS[i])

# 4. Plotting the Number of Edges a Graph Has Versus the Number of Subsets of Edges We Will Need to Check to Find a Maximum Matching Using a Naive Approach
xs <- 1:12
ys <- 2^xs
plot(xs, ys, pch=4, cex=1.5, lwd=3, col="#009ed8",
     xlab="Number of Edges of Graph", ylab="Number of Subsets to Check")

# 5. Finding Augmenting Paths
augmenting_path <- function(adj_list, match_vec) {
  vertices_match_status <- match_vec != 0 # we create a logical vector to represent whether each vertex is included in the matching
  num_vertices <- length(vertices_match_status) # we find the number of vertices in the graph
  unmatched_vertices <- which(vertices_match_status == FALSE, arr.ind=TRUE) # we create a vector of the vertices which are not in the matching
  for (i in 1:num_vertices) { # we will try finding an augmenting path starting from each unmatched vertex until we find one
    aug_path <- matrix(, nrow=0, ncol=2) # we initialise an empty matrix to start the augmenting path 
    if (vertices_match_status[i] == TRUE) { # we skip the matched vertices
      next 
    }
    root <- i
    current_vert <- i # we will use this to keep track of which vertex we have reached in our attempt to find an augmenting path
    # Let's keep track of the vertices we've included in our augmenting path attempt from this vertex:
    unvisited_from_root <- c(1:num_vertices)
    unvisited_from_root <- unvisited_from_root[!unvisited_from_root==root]
    repeat { # we create a repeating loop that will run the same procedure until we break out of it using break or return
      neighbours <- unlist(adj_list[[current_vert]]) # we create a vector of the vertices that have an edge between them and the vertex that we are currently on 
      neighbours_unvisited_from_root <- intersect(neighbours, unvisited_from_root) # we create a vector of vertices adjacent to the vertex we are currently on that have not already been included in our attempt to find an augmenting path
      if (length(neighbours_unvisited_from_root) == 0) { # if there are none of these, we can stop running our repear loop and try our searh again from the next unmatched vertex
        break
      }
      match_to_current_vert <- match_vec[current_vert] 
      # if we are on a vertex that is part of the matching, and the vertex it is matched to is not yet part of the augmenting path, we want to go to that vertex next 
      if (match_to_current_vert %in% unvisited_from_root) { 
        next_vert <- match_to_current_vert
        unvisited_from_root <- unvisited_from_root[!unvisited_from_root==next_vert]
        aug_path <- rbind(aug_path, c(current_vert,next_vert))
        current_vert <- next_vert
        next
      }
      unmatched_neighbours_unvisited_from_root <- intersect(unmatched_vertices, neighbours_unvisited_from_root) # we create a vector of unmatched vertices adjacent to the vertex we are currently on that we haven't already included in our augmenting path attempt
      if (length(unmatched_neighbours_unvisited_from_root) > 0) { # if any of these exist, then we have found an augmenting path, and we can return it!
        aug_path <- rbind(aug_path, c(current_vert,unmatched_neighbours_unvisited_from_root[1]))
        return(aug_path)
      }
      # If we didn't return an augmenting path in the if statement above, all the vertices adjacent to the vertex we are currently on that are not already included in our augmenting path attempt must be part of the matching, so we will go to one of these next and continue searching:
      next_vert <- neighbours_unvisited_from_root[1]
      unvisited_from_root <- unvisited_from_root[!unvisited_from_root==next_vert]
      aug_path <- rbind(aug_path, c(current_vert,next_vert))
      current_vert <- next_vert
    }
  }
  # If we haven't returned an augmenting path after searching from all unmatched vertices, we return an empty matrix:
  aug_path <- matrix(, nrow=0, ncol=2)
  return(aug_path) 
}
example_adj_mat <- matrix(c(0,1,0,0,
                            1,0,1,0,
                            0,1,0,1,
                            0,0,1,0), nrow = 4, byrow = TRUE)
example_adj_list <- adj_mat_to_adj_list(example_adj_mat)
example_match <- c(0,3,2,0)
augmenting_path(example_adj_list, example_match)

# 6. Finding Maximum Matchings
# Let's use our augmenting_path function to write a Hungarian Maximum Matching Algorithm that takes as its input the adjacency matrix of a graph and a matching vector:
maximum_matching <- function(adj_mat, match_vec) {
  adj_list <- adj_mat_to_adj_list(adj_mat) # we use the function we wrote earlian 
  num_vertices <- length(match_vec) # we find the number of vertices in the graph
  # Let's create a matrix to store the current matching:
  current_matching <- matrix(, nrow=0, ncol=2)
  # We can populate it with the initial matching to start with:
  for (i in 1:num_vertices) {
    if (match_vec[i] == 0) {
      next
    }
    current_matching <- rbind(current_matching, c(i,match_vec[i]))
  }
  if (nrow(current_matching) != 0) {
    current_matching <- unique(t(apply(current_matching, 1, sort))) # we order the edges with the lower indexed vertex first, and remove duplicates
  }
  # Now we will run our augmenting path algorithm and and use it to increase our current matching until no more augmenting paths can be found:
  current_match_vec <- match_vec
  repeat {
    aug_path <- augmenting_path(adj_list, current_match_vec)
    aug_path_length <- nrow(aug_path)
    if (aug_path_length == 0) {
      break
    }
    aug_path <- t(apply(aug_path, 1, sort)) # we order the edges in the augmenting path with the lower indexed vertex first
    if (aug_path_length == 1) {
      current_matching <- rbind(current_matching, aug_path)
      current_matching_length <- nrow(current_matching)
      current_match_vec <- rep(0,6)
      for (i in 1:current_matching_length) {
        current_match_vec[current_matching[i,1]] <- current_matching[i,2]
        current_match_vec[current_matching[i,2]] <- current_matching[i,1]
      }
      next
    }
    for (i in 1:aug_path_length) {
      if (i%%2 == 1) {
        current_matching <- rbind(current_matching, aug_path[i,])
      }
      if (i%%2 == 0) {
        current_matching <- current_matching[!(current_matching[,1]==aug_path[i,1] & current_matching[,2]==aug_path[i,2]),]
      }
    }
    current_match_vec <- rep(0,6)
    current_matching_length <- nrow(current_matching)
    for (i in 1:current_matching_length) {
      current_match_vec[current_matching[i,1]] <- current_matching[i,2]
      current_match_vec[current_matching[i,2]] <- current_matching[i,1]
    }
  }
  return(current_matching)
}
S_empty_matching <- rep(0,6)
S_max_matching <- maximum_matching(S_adj_mat, S_empty_matching)
apply(S_max_matching, 1:2, function(i) LETTERS[i])