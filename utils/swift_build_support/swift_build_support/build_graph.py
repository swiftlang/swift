# swift_build_support/build_graph.py ----------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# This is a simple implementation of an acyclic build graph. We require no
# cycles, so we just perform a reverse post order traversal to get a topological
# ordering. We check during the reverse post order traversal that we do not
# visit any node multiple times.
#
# Nodes are assumed to be a product's class.
#
# ----------------------------------------------------------------------------


def _get_po_ordered_nodes(root, invertedDepMap):
    # Then setup our worklist/visited node set.
    worklist = [root]
    visitedNodes = set([])
    # TODO: Can we unify po_ordered_nodes and visitedNodes in some way?
    po_ordered_nodes = []

    # Until we no longer have nodes to visit...
    while not len(worklist) == 0:
        # First grab the last element of the worklist. If we have already
        # visited this node, just pop it and skip it.
        #
        # DISCUSSION: Consider the following build graph:
        #
        #   A -> [C, B]
        #   B -> [C]
        #
        # In this case, we will most likely get the following worklist
        # before actually processing anything:
        #
        #   A, C, B, C
        #
        # In this case, we want to ignore the initial C pushed onto the
        # worklist by visiting A since we will have visited C already due to
        # the edge from B -> C.
        node = worklist[-1]
        if node in visitedNodes:
            worklist.pop()
            continue

        # Then grab the dependents of our node.
        deps = invertedDepMap.get(node, set([]))
        assert(isinstance(deps, set))

        # Then visit those and see if we have not visited any of them. Push
        # any such nodes onto the worklist and continue. If we have already
        # visited all of our dependents, then we can actually process this
        # node.
        foundDep = False
        for d in deps:
            if d not in visitedNodes:
                foundDep = True
                worklist.append(d)
        if foundDep:
            continue

        # Now process the node by popping it off the worklist, adding it to
        # the visited nodes set, and append it to the po_ordered_nodes in
        # its final position.
        worklist.pop()
        visitedNodes.add(node)
        po_ordered_nodes.append(node)
    return po_ordered_nodes


class BuildDAG(object):

    def __init__(self):
        self.root = None

        # A map from a node to a list of nodes that depend on the given node.
        #
        # NOTE: This is an inverted dependency map implying that the root will
        # be a "final element" of the graph.
        self.invertedDepMap = {}

    def add_edge(self, pred, succ):
        self.invertedDepMap.setdefault(pred, set([succ])) \
                           .add(succ)

    def set_root(self, root):
        # Assert that we always only have one root.
        assert(self.root is None)
        self.root = root

    def produce_schedule(self):
        # Grab the root and make sure it is not None
        root = self.root
        assert(root is not None)

        # Then perform a post order traversal from root using our inverted
        # dependency map to compute a list of our nodes in post order.
        #
        # NOTE: The index of each node in this list is the post order number of
        # the node.
        po_ordered_nodes = _get_po_ordered_nodes(root, self.invertedDepMap)

        # Ok, we have our post order list. We want to provide our user a reverse
        # post order, so we take our array and construct a dictionary of an
        # enumeration of the list. This will give us a dictionary mapping our
        # product names to their reverse post order number.
        rpo_ordered_nodes = list(reversed(po_ordered_nodes))
        node_to_rpot_map = dict((y, x) for x, y in enumerate(rpo_ordered_nodes))

        # Now before we return our rpo_ordered_nodes and our node_to_rpot_map, lets
        # verify that we didn't find any cycles. We can do this by traversing
        # our dependency graph in reverse post order and making sure all
        # dependencies of each node we visit has a later reverse post order
        # number than the node we are checking.
        for n, node in enumerate(rpo_ordered_nodes):
            for dep in self.invertedDepMap.get(node, []):
                if node_to_rpot_map[dep] < n:
                    print('n: {}. node: {}.'.format(n, node))
                    print('dep: {}.'.format(dep))
                    print('inverted dependency map: {}'.format(self.invertedDepMap))
                    print('rpo ordered nodes: {}'.format(rpo_ordered_nodes))
                    print('rpo node to rpo number map: {}'.format(node_to_rpot_map))
                    raise RuntimeError('Found cycle in build graph!')

        return (rpo_ordered_nodes, node_to_rpot_map)


def produce_scheduled_build(input_product_classes):
    """For a given a subset input_input_product_classes of
       all_input_product_classes, compute a topological ordering of the
       input_input_product_classes + topological closures that respects the
       dependency graph.
    """
    dag = BuildDAG()
    worklist = list(input_product_classes)
    visited = set(input_product_classes)

    # Construct the DAG.
    while len(worklist) > 0:
        entry = worklist.pop()
        deps = entry.get_dependencies()
        if len(deps) == 0:
            dag.set_root(entry)
        for d in deps:
            dag.add_edge(d, entry)
            if d not in visited:
                worklist.append(d)
        visited = visited.union(deps)

    # Then produce the schedule.
    schedule = dag.produce_schedule()

    # Finally check that all of our input_product_classes are in the schedule.
    if len(set(input_product_classes) - set(schedule[0])) != 0:
        raise RuntimeError('Found disconnected graph?!')

    return schedule
