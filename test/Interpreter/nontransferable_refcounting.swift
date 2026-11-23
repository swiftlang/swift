// RUN: %target-run-simple-swift(-Onone -enable-experimental-feature NontransferableTypes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NontransferableTypes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NontransferableTypes

// The reference counts of instances of a non-'Transferable' class, of the
// contexts of @nontransferable functions and of boxes of non-'Transferable'
// values are updated non-atomically. Check that they are still balanced,
// including when values are copied through value witnesses and mixed with
// 'Transferable' instances.

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

func testCapturedVar() {
  var nt = NT(8)
  let f: @nontransferable () -> Void = {
    nt = NT(9)
    print("captured \(nt.id)")
  }
  let (a, b) = duplicate(f)
  a()
  b()
  print("after \(nt.id)")
}

indirect enum List: ~Transferable {
  case end
  case cons(NT, List)
}

@inline(never)
func count(_ list: List) -> Int {
  switch list {
  case .end:
    return 0
  case .cons(let nt, let rest):
    return nt.id + count(rest)
  }
}

func testList() {
  let list = List.cons(NT(10), .cons(NT(11), .end))
  let (a, b) = duplicate(list)
  print("list \(count(a)) \(count(b))")
}

enum ConditionalList<Element: ~Transferable>: ~Transferable {
  case end
  indirect case cons(Element, ConditionalList)
}

extension ConditionalList: Transferable where Element: Transferable {}

@inline(never)
func count(_ list: ConditionalList<NT>) -> Int {
  switch list {
  case .end:
    return 0
  case .cons(let nt, let rest):
    return nt.id + count(rest)
  }
}

// Concrete code counts the boxes of 'ConditionalList<NT>' non-atomically, the
// value witnesses that generic code uses count them atomically.
func testConditionalList() {
  let list = ConditionalList.cons(NT(12), .cons(NT(13), .end))
  let copy = list
  let (a, b) = duplicate(list)
  print("conditional list \(count(copy)) \(count(a)) \(count(b))")
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

// CHECK-NEXT: deinit NT 8
// CHECK-NEXT: captured 9
// CHECK-NEXT: deinit NT 9
// CHECK-NEXT: captured 9
// CHECK-NEXT: after 9
// CHECK-NEXT: deinit NT 9
testCapturedVar()
print("done captured var")
// CHECK-NEXT: done captured var

// CHECK-NEXT: list 21 21
// CHECK-DAG: deinit NT 10
// CHECK-DAG: deinit NT 11
testList()
print("done list")
// CHECK: done list

// CHECK-NEXT: tree 11 11
// CHECK-DAG: deinit NT 5
// CHECK-DAG: deinit NT 6
testTree()
print("done tree")
// CHECK: done tree

// CHECK-NEXT: conditional list 25 25 25
// CHECK-DAG: deinit NT 12
// CHECK-DAG: deinit NT 13
testConditionalList()
print("done conditional list")
// CHECK: done conditional list
