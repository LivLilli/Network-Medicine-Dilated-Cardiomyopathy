library(igraph)
library(qgraph)
library(MCL)


'
Beacuse of the better computational time, we computed avg shortest path, 
radius and diameter for the llcs with R igraph package
'


'
task_2_1_b_global()

Input:

    - adjacency matrix

Returns:

    - df with radius, diameter and average shortest path
'

task_2_1_b_global = function(a_matrix){
  
  # build graph from adj matrix
  graph = graph_from_adjacency_matrix(a_matrix,mode = "undirected")
  # avg shortest path
  avg_s_p = average.path.length(graph, directed = FALSE)
  # diameter
  diameter = diameter(graph, directed = FALSE)
  # radius
  radius = radius(graph)
  
  
  
}


# task 2.2

mcl(matrix, 2)


