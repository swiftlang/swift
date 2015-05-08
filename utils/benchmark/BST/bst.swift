// A binary search tree.
//
// TODO: make it generic once we handle the basics.
class BST {
  class Node {
    var left:Node?
    var right:Node?
    var key:Int = 0
    
    init(_ key:Int) {
      self.key = key
    }
  }

  var root:Node?
    
  func insert(key:Int) -> Bool {
    if !root {
      root = BST.Node(key)
      return true
    }
    return insertInto(key, node: root!)
  }

  // Written this way for tail recursion.
  func insertInto(key:Int, node:Node) -> Bool {
    if key == node.key {
      return false
    }
    if key < node.key {
      if node.left {
        return insertInto(key, node: node.left!)
      } else {
        node.left = BST.Node(key)
        return true
      }
    } else {
      if node.right {
        return insertInto(key, node: node.right!)
      } else {
        node.right = BST.Node(key)
        return true
      }
    }
  }

  func find(key:Int) -> Bool {
    return findIn(key, node: root)
  }

  func findIn(key:Int, node:Node?) -> Bool {
    if !node {
      return false
    }
    let n = node!
    if key == n.key {
      return true
    }
    if key < n.key {
      return self.findIn(key, node: n.left)
    } else {
      return self.findIn(key, node: n.right)
    }
  }

  func minKey() -> Int? {
    if !root {
      return nil
    }
    var node = root!
    while (node.left) {
      node = node.left!
    }
    return node.key
  }

  func maxKey() -> Int? {
    if !root {
      return nil
    }
    var node = root!
    while (node.right) {
      node = node.right!
    }
    return node.key
  }

  func depth() -> Int {
    return depthIn(root)
  }

  func depthIn(node:Node?) ->Int {
    if !node {
      return 0
    }
    let n = node!
    let leftDepth = depthIn(n.left)
    let rightDepth = depthIn(n.right)
    return max(leftDepth, rightDepth) + 1
  }        

  func size() -> Int {
    return sizeIn(root)
  }

  func sizeIn(node:Node?) -> Int {
    if let n = node {
      return 1 + sizeIn(n.left) + sizeIn(n.right)
    }
    return 0
  }

  func visitInorder(f:(Int) -> ()) {
    self.visitInorderSubtree(f, node: root)
  }
  func visitInorderSubtree(f:(Int) -> (), node:Node?) {
    if let n = node {
      self.visitInorderSubtree(f, node: n.left)
      f(n.key)
      self.visitInorderSubtree(f, node: n.right)
    }
  }
}

import liblfsr

func test(N:Int) {
  var bst = BST()
  var lfsr = LFSR()
  for i in 0..N {
    bst.insert(lfsr.randInt())
  }
  print("Size \(bst.size())")
  print("Depth \(bst.depth())")
  if let min = bst.minKey() {
    let max = bst.maxKey()!
    print("Range \(min) - \(max)")
  }
  print("Values: ")
  bst.visitInorder({print("\($0) ")})
  print("")
}

// I'm embarassed that I don't know how to read cmd line args.
test(1000)
