#!/usr/bin/env python
#
# A binary search tree.
class BST:
    class Node:
        left, right, key = None, None, 0
    
        def __init__(self, key):
            self.left = None
            self.right = None
            self.key = key

    def __init__(self):
        self.root = None
    
    def insert(self, key):
        if not self.root:
            self.root = BST.Node(key)
            return True
        return self.insertInto(key, self.root)

    def insertInto(self, key, node):
        if key == node.key:
            return False
        if key < node.key:
            if node.left:
                return self.insertInto(key, node.left)
            else:
                node.left = BST.Node(key)
                return True
        else:
            if node.right:
                return self.insertInto(key, node.right)
            else:
                node.right = BST.Node(key)
                return True

    def find(self, key):
        return self.findIn(key, self.root)

    def findIn(self, key, node):
        if not node:
            return False
        if key == node.key:
            return True
        if key < node.key:
            return self.findIn(node.left, key)
        else:
            return self.findIn(node.right, key)

    def minKey(self):
        if not self.root:
            return None
        node = self.root
        while (node.left):
            node = node.left
        return node.key

    def maxKey(self):
        if not self.root:
            return None
        node = self.root
        while (node.right):
            node = node.right
        return node.key

    def depth(self):
        return self.depthIn(self.root)
        
    def depthIn(self, node):
        if not node:
            return 0
        leftDepth = self.depthIn(node.left)
        rightDepth = self.depthIn(node.right)
        return max(leftDepth, rightDepth) + 1
            
    def size(self):
        return self.sizeIn(self.root)
    
    def sizeIn(self, node):
        if not node:
            return 0
        return 1 + self.sizeIn(node.left) + self.sizeIn(node.right)

    def visitInorder(self, f):
        self.visitInorderSubtree(f, self.root)

    def visitInorderSubtree(self, f, node):
        if not node:
            return
        self.visitInorderSubtree(f, node.left)
        f(node.key)
        self.visitInorderSubtree(f, node.right)

def printKey(key):
    print key,

def usage():
    print "Usage: bst <numkeys>"

if __name__ == "__main__":
    import sys, lfsr
    if len(sys.argv) != 2:
        usage()
        sys.exit(1)

    bst = BST()
    lfsr = lfsr.LFSR()
    for i in range(0,int(sys.argv[1])):
        bst.insert(lfsr.randInt())
    
    print "Size:", bst.size()
    print "Depth:", bst.depth()
    print "Range:", bst.minKey(), "-", bst.maxKey()
    print "Values:",
    bst.visitInorder(printKey)
