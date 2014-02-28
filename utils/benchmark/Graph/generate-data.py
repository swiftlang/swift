
import pygraph.algorithms.generators as gen
import pygraph.algorithms.accessibility as acc
import pygraph.algorithms.minmax as minmax

graph = gen.generate(5000, 10000, weight_range=(50, 2000))
components = acc.connected_components(graph)
nodes = [g for g in graph if components[g] == 1]

print "GRAPH NODES"
for n in graph.nodes():
    print n
print "GRAPH EDGES"
for e in graph.edges():
    if components[e[0]] == 1:
        w = graph.edge_weight(e)
        print (e[0], e[1], w)

# MST = minmax.minimal_spanning_tree(graph)
# print "MST NODES"
# for n in MST.keys():
#      print n
# print "MST EDGES"
# for k in MST.keys():
#     if MST[k] is not None:
#         print "(%d, %d)" % (k, MST[k])
#     else:
#         print "(%d, %d)" % (k, k)

