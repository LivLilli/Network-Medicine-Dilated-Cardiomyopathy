library(igraph)

### centralization function
centralization = function(graph){
  # centralization btw based
  btw_centr = centr_betw(graph)$centralization
  # centralization closeness based
  #clos_centr = centr_clo(graph)$centralization
  # centralization eigen based
  eigen_centr = centr_eigen(graph)$centralization 
  # centralization degree based
  degree_centr = centr_degree(graph)$centralization
  df = data.frame(btw_centr, eigen_centr, degree_centr)
  colnames(df) = c('Btw Centralization', 'Eigen Centralization', 'Degree Centralization')
  return(df)
}


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

### load adjacency matrices
intersection = as.matrix(read.table("data/task_2_1_intersection_df_adj_matrix.csv", sep=",", header = TRUE, row.names = 1))
sgi =  as.matrix(read.table("data/task_2_1_sgi_adj_matrix.csv", sep=",", header = TRUE, row.names = 1))
union =  as.matrix(read.table("data/task_2_1_union_adj_matrix.csv", sep=",", header = TRUE, row.names = 1))
### build graphs from adj matrices
i_graph = graph_from_adjacency_matrix(intersection,mode = "undirected")
s_graph = graph_from_adjacency_matrix(sgi,mode = "undirected")
u_graph = graph_from_adjacency_matrix(union,mode = "undirected")


### avg shortest path, diameter and radius for union graph
# avg sp
avg_s_p = average.path.length(u_graph, directed = FALSE, unconnected = TRUE)
# diameter
diameter = diameter(u_graph, directed = FALSE)
# radius
radius = radius(u_graph)



### create partial union df
partial_union1 = data.frame(avg_s_p, diameter, radius)
colnames(partial_union1) = c('Avg Shortest Path', 'Diameter', 'Radius')
partial_union2 = centralization(u_graph)
partial_union = t(rbind(t(partial_union1), t(partial_union2)))

### create partial sgi and intersection df
partial_sgi = centralization(s_graph)
partial_intersection = centralization(i_graph)

### save partial dataframes in order to merge them with the final results on python

save_to_csv(partial_union, 'data/task_2_1_a_partial_union.csv')
save_to_csv(partial_sgi, 'data/task_2_1_a_partial_sgi.csv')
save_to_csv(partial_intersection, 'data/task_2_1_a_partial_intersection.csv')
