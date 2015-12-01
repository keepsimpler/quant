###############################################################################
#' @title Generate a connected graph using package [igraph]
#'
#' @param s, size of network.
#' if graph type is bipartite, s[1], s[2] represent size of two groups; else s is size of network
#' @param k, average degree for the network.
#' 1 < k < s for unipartite network, 1 < k < s[1]*s[2]/(s[1]+s[2]) for bipartite network.
#' @param type, Graph type generated: 'bipartite', 'sf', 'er', 'dag', 'regular'.
#' @param maxtried, the maximum number of tried times.
#' If have tried [maxtried] times, the function will return no matter whether the connected graph is generated.
#' @param exponent exponent coefficient of Scale-Free network
#' @param ... the params conform to the implementation functions of [igraph]
#' @return the connected graph
#' @details .
#' @import igraph
gen_connected_graph <- function(s, k, type, maxtried = 100, exponent = 2.5, ...) {
  #library(igraph)
  if (type == 'bipartite' && is.na(s[2])) {  # the bipartite graph need size of two groups of nodes
    warning('sizes of TWO groups of nodes should be designated.
            we have assumed the size of second group equal to the size of first group.')
    s[2] = s[1]  # if missed second size, we assume it equal to the first size.
  }
  count = 0
  repeat {  # generate a connected graph
    if (type == 'bipartite') {
      G = sample_bipartite(s[1], s[2], type = 'gnm', m = ceiling(k * (s[1] + s[2])))
    } else if (type == 'sf') {
      G = sample_fitness_pl(s, k * s, exponent.out = exponent)
    }
    else if (type == 'er') {
      G = sample_gnm(s, k * s)
    }
    else if (type == 'regular') {
      G = sample_k_regular(s, k)
    }
    else if (type == 'complete') {
      G = make_full_graph(s)
    }
    else if (type == 'dag') {
      require('spacejam')  # generate random directed Acyclic graphs
      G = rdag(s, s * k * 2)
    }
    if (igraph::is.connected(G)) break  # until a connected graph is generated
    count = count + 1
    if (count == maxtried) {
      warning(paste('Tried', maxtried, 'times, But connected graph still cannot be generated.'))
      break
    }
  }
  G
}
#plot(G, layout = layout.bipartite)


#' @title generate a hybrid network that include competition, antagonism, mutualism interactions
#' @param s number of species
#' @param k average degree of species
#' @param type network type, 'er':random graph, 'sf':scale-free, 'bipartite':bipartite graph, 'niche':niche model
#' @param pc probability of competition interactions
#' @param pa probability of antagonism interactions
#' @param pm probability of mutualism interactions
#' @param ... additional arguments transformed to graph generate such as [exponent]
gen_hybrid_network <- function(s, k, type = 'er', pc = 0., pa = 0., pm = 1., ...) {
  stopifnot(pc >= 0., pa >= 0., pm >= 0., round(pc + pa + pm, 5) == 1)
  G = gen_connected_graph(s, k, type, ...)  # generate a connected graph
  graph = as.matrix(get.adjacency(G))  # transform to matrix form
  # split the graph to three sub-graph - competition, antagonism and mutualism graphs according to the probability of occurance of three different types of interactions
  competitive_graph = matrix(0, nrow = s, ncol = s)
  antago_graph = matrix(0, nrow = s, ncol = s)
  mutual_graph = matrix(0, nrow = s, ncol = s)
  ps = runif(sum(graph))
  cursor = 0
  for (i in 2:s) {
    for (j in 1:(i-1)) {
      if (graph[i, j] == 1) { # if an undirected edge exists between i and j
        cursor = cursor + 1
        p = ps[cursor]
        if (p < pc) {  # competitive sub-graph
          competitive_graph[i, j] = 1
          competitive_graph[j, i] = 1
        }
        else if (p < pc + pa / 2) { # antagonism sub-graph
          antago_graph[i, j] = 1
          antago_graph[j, i] = - 1
        }
        else if (p < pc + pa) { # antagonism sub-graph
          antago_graph[i, j] = - 1
          antago_graph[j, i] = 1
        }
        else if ( p < pc + pa + pm) { # mutualism sub-graph
          mutual_graph[i, j] = 1
          mutual_graph[j, i] = 1
        }
      }
    }
  }
  list(competitive_graph = competitive_graph, antago_graph = antago_graph, mutual_graph = mutual_graph)
}

#' @title another form of uniform distribution between [mean - sd, mean + sd]
runif2 <- function(n, mean, sd) {
  runif(n) * 2 * sd + (mean - sd)
}
