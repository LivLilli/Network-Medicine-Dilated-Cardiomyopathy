library(igraph)
library(qgraph)
library(MCL)
library(data.table)

'
save_to_csv()

  Input: dataframe of local/global properties; file name on which to save the df.

  Output: csv file containing the dataframe.
'

save_to_csv = function(df, name){
  if (file.exists(name)) 
    #Delete file if it exists
    file.remove(name)
  
  write.csv(df, name)
}


'
task_2_1_b_global()

  Input: adjacency matrix.

  Output: df with avg sp, diameter, radius, centralization of the lcc.
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
  # centralization btw based
  btw_centr = centr_betw(graph)$centralization
  # centralization closeness based
  clos_centr = centr_clo(graph)$centralization
  # centralization eigen based
  eigen_centr = centr_eigen(graph)$centralization 
  # centralization degree based
  degree_centr = centr_degree(graph)$centralization
  df = data.frame(avg_s_p, diameter, radius, btw_centr, clos_centr, eigen_centr, degree_centr)
  colnames(df) = c('Avg Shortest Path', 'Diameter', 'Radius', 'Btw Centralization', 
                  'Closeness Centralization', 'Eigen Centralization', 'Degree Centralization')
  return(df)
  
}


' 
task_2_1_b_local()

  Input: adjacency matrix.

  Output: dataframe with local properties (degree, btw centrality, eigen centrality
    closeness centrality, ratio among btw and degree) for each node.
'
task_2_1_b_local = function(a_matrix){
  # build graph from adj matrix
  graph = graph_from_adjacency_matrix(a_matrix,mode = "undirected")
  # degree vector
  degrees = degree(graph)
  # btw centrality vector: value for each node
  btw_centrality = betweenness(graph, directed = FALSE, normalized = TRUE)
  # eigenvector centrality
  eig_centrality = eigen_centrality(graph, directed = FALSE, scale = FALSE)$vector
  # closeness centrality vector
  clos_centrality = closeness(graph, normalized = TRUE)
  # ratio btw/degree
  ratio = btw_centrality/degrees
  df = data.frame(degrees, btw_centrality, eig_centrality, clos_centrality, ratio)
  colnames(df) = c('Degree', 'Betweenness Centrality', 'Eigen Centrality', 'Closeness Centrality', 'Ratio Btw/Degree')
  return(df)
}



### TASK 2.1 B - GLOBAL
# load adjacency matrix for union and intersection graphs
lcc_union_matrix = as.matrix(read.table("data/union_lcc_matrix.csv", sep=",", header = TRUE, row.names = 1))
lcc_intersection_matrix = as.matrix(read.table("data/intersection_lcc_matrix.csv", sep=",", header = TRUE, row.names = 1))         
# compute df of global properties for union and intersection lcc
union_global_df = task_2_1_b_global(lcc_union_matrix)
inters_global_df = task_2_1_b_global(lcc_intersection_matrix)


### TASK 2.1 B - LOCAL
# compute df of local properties for intersection and union lcc
inters_local_df = task_2_1_b_local(lcc_intersection_matrix)
union_local_df = task_2_1_b_local(lcc_union_matrix)



### save resulting dataframes
save_to_csv(inters_local_df, 'data/intersection_lcc_local_results.csv')
save_to_csv(union_local_df, 'data/union_lcc_local_results.csv')
save_to_csv(inters_global_df, 'data/intersection_lcc_global_results.csv')
save_to_csv(union_global_df, 'data/union_lcc_global_results.csv')

