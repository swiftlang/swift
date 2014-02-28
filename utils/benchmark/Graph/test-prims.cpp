
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <map>

#include "graph.h"
using namespace graph;

static bool scan_header(FILE *f, const char *str1, const char *str2) {
  char buf1[128], buf2[128];
  if (2 != fscanf(f, "%s %s\n", buf1, buf2) ||
      strcmp(buf1, str1) != 0 ||
      strcmp(buf2, str2) != 0) {
    return false;
  }

  return true;
}

int main(int argc, char *argv[]) {
  FILE *f = fopen("data", "r");

  // Parse the nodes from stdin.
  std::vector<Node *> nodes;

  if (!scan_header(f, "GRAPH", "NODES")) {
    printf("Bad input format.\n");
    exit(-1);
  }

  unsigned node_id;
  while (1 == fscanf(f, "%u\n", &node_id)) {
    printf("Found Node: %u\n", node_id);
    nodes.push_back(new Node(node_id));
  }

  if (!scan_header(f, "GRAPH", "EDGES")) {
    printf("Bad input format.\n");
    exit(-1);
  }

  printf("Creating edges.\n");
  unsigned node_id_1, node_id_2;
  int weight;
  std::map<std::pair<unsigned, unsigned>, double> weight_map;
  while (3 == fscanf(f, "(%u, %u, %d)\n", &node_id_1, &node_id_2, &weight)) {
    printf("%u -> %u. Weight: %d\n", node_id_1, node_id_2, weight);
    nodes[node_id_1]->adjList.push_back(node_id_2);
    weight_map[{node_id_1, node_id_2}] = weight;
  }

#if 0
  if (!scan_header(f, "MST", "NODES")) {
    printf("Bad input format.\n");
    exit(-1);
  }

  while (1 == fscanf(f, "%u\n", &node_id)) {
    printf("Found MST Node: %u\n", node_id);
  }

  if (!scan_header(f, "MST", "EDGES")) {
    printf("Bad input format.\n");
    exit(-1);
  }

  std::vector<std::pair<unsigned, unsigned>> Result;
  while (2 == fscanf(f, "(%u, %u)\n", &node_id_1, &node_id_2)) {
    //printf("%u -> %u. Weight: %d\n", node_id_1, node_id_2, weight);    
    Result.push_back({node_id_1, node_id_2});
  } 
#endif

  // Our closure uses our weight_map to look up the weight in between ndoeindex
  // and adj_node_index.
  std::vector<unsigned> TreeEdges;
  prims(nodes, TreeEdges, [&weight_map] (unsigned ind1, unsigned ind2) -> double {
    return weight_map[{ind1, ind2}];
  });

#if 0
  // Check that we got the same result.
  assert(TreeEdges.size() == Result.size() && "Expected and Actual MST differ "
         "in size.");
  double expected_weight = 0.0, actual_weight = 0.0;
  for (unsigned i = 0; i < Result.size(); i++) {
    if (TreeEdges[i] != -1U) {
      if (TreeEdges[i] != Result[i].second)
        printf("%u -> (%u, %u)\n", i, TreeEdges[i], Result[i].second);
      expected_weight += weight_map[{Result[i].first, Result[i].second}];
      actual_weight += weight_map[{TreeEdges[i], i}];
      assert(TreeEdges[i] == Result[i].second);
    }
  }

  printf("Weight: %.0f, %.0f\n", expected_weight, actual_weight);
#endif

  // Cleanup.
  for (auto *n : nodes)
    delete n;

  fclose(f);

  return 0;
}
