// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature NontransferableTypes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NontransferableTypes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NontransferableTypes

// The reference counts of instances of a non-'Transferable' class and of the
// contexts of @nontransferable functions are updated non-atomically. Check
// that they are still balanced, including when values are copied through
// value witnesses and mixed with 'Transferable' instances.

final class NT: ~Transferable {
  let id: Int
  init(_ id: Int) { self.id = id }
  deinit { print("deinit NT \(id)") }
}

final class T {
  let id: Int
  init(_ id: Int) { self.id = id }
  deinit { print("deinit T \(id)") }
}

struct Single: ~Transferable {
  var nt: NT
}

struct Mixed: ~Transferable {
  var nt: NT
  var t: T
}

enum Tree<Value>: ~Transferable {
  case leaf(Value)
  indirect case node(NT, Tree, Tree)
}

@inline(never)
func duplicate<V: ~Transferable>(_ value: borrowing V) -> (V, V) {
  (copy value, copy value)
}

@inline(never)
func sum<Value>(_ tree: Tree<Value>) -> Int {
  switch tree {
  case .leaf:
    return 0
  case .node(let nt, let left, let right):
    return nt.id + sum(left) + sum(right)
  }
}

func testCopies() {
  let nt = NT(1)
  let (a, b) = duplicate(nt)
  print("copies \(nt.id) \(a.id) \(b.id)")
}

func testSingle() {
  let single = Single(nt: NT(2))
  let (a, b) = duplicate(single)
  print("single \(a.nt.id) \(b.nt.id)")
}

func testMixed() {
  let mixed = Mixed(nt: NT(3), t: T(3))
  let (a, b) = duplicate(mixed)
  print("mixed \(a.nt.id) \(b.t.id)")
}

func testUnowned() {
  let nt = NT(4)
  unowned let u = nt
  print("unowned \(u.id)")
}

func testClosures() {
  let nt = NT(7)
  let t = T(7)
  let f: @nontransferable () -> Void = { print("closure NT \(nt.id)") }
  let g = f
  let (a, b) = duplicate(f)
  let converted: @nontransferable () -> Void = { print("closure T \(t.id)") }
    as () -> Void
  g()
  a()
  b()
  converted()
}

func testTree() {
  let tree = Tree<Int>.node(NT(5), .leaf(0), .node(NT(6), .leaf(0), .leaf(0)))
  let (a, b) = duplicate(tree)
  print("tree \(sum(a)) \(sum(b))")
}

// CHECK: copies 1 1 1
// CHECK-NEXT: deinit NT 1
testCopies()
print("done copies")
// CHECK-NEXT: done copies

// CHECK-NEXT: single 2 2
// CHECK-NEXT: deinit NT 2
testSingle()
print("done single")
// CHECK-NEXT: done single

// CHECK-NEXT: mixed 3 3
// CHECK-DAG: deinit NT 3
// CHECK-DAG: deinit T 3
testMixed()
print("done mixed")
// CHECK: done mixed

// CHECK-NEXT: unowned 4
// CHECK-NEXT: deinit NT 4
testUnowned()
print("done unowned")
// CHECK-NEXT: done unowned

// CHECK-NEXT: closure NT 7
// CHECK-NEXT: closure NT 7
// CHECK-NEXT: closure NT 7
// CHECK-NEXT: closure T 7
// CHECK-DAG: deinit NT 7
// CHECK-DAG: deinit T 7
testClosures()
print("done closures")
// CHECK: done closures

// CHECK-NEXT: tree 11 11
// CHECK-DAG: deinit NT 5
// CHECK-DAG: deinit NT 6
testTree()
print("done tree")
// CHECK: done tree
