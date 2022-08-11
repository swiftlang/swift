class Tree {
  var left: Tree
  var right: Tree

  var value: Int

  init(left: Tree, right: Tree) {
    self.left = left
    self.right = right
    self.value = 0
  }
}

@inline(never)
func useOwned(_ x: __owned Tree) { }
@inline(never)
func useShared(_ x: __shared Tree) {
  useOwned(x)
}

useShared(getTree())

@inline(never)
func getTree() -> Tree { fatalError() }

@inline(never)
func storeMaybeLocal(_ t: Tree) {
  let target = t.value > 10 ? t : Tree(left: getTree(), right: getTree())
  target.value = 0
}

storeMaybeLocal(getTree())

@inline(never)
func store0(_ left: Bool, _ to: Tree) -> Int {
  let target = left ? to.left : to.right
  let prev = target.value
  target.value = 0
  return prev
}

// store0(true, getTree())

struct Pair {
  var x: Int = 0
  var y: Int = 0

  var foo: Int {
    get {
      x
    }

    @inline(never)
    _modify {
      yield &y
    }
  }
}

class List {
  var next: List?
  var elem: Pair = Pair()
}

@inline(never)
func writeToSecond(_ l: List) {
  if let next = l.next {
    next.elem.x = 0
  } else {
    l.elem.x = 0
  }
}

var me = List()
me.next = me
writeToSecond(me)

@inline(never)
func addressorTest() {
  var p = Pair()
  // The following is a call to the `foo._modify` addressor
  // which returns an inner address of `p`
  p.foo = 10
}

protocol Proto {
  __consuming func mutatingMethod()
}

public class KlassProto : Proto {
  var x: Int
  init() { x = 0 }
  func mutatingMethod() {
    x = 5
  }
}

class ChildProto : KlassProto {}

@inline(never)
func getProto() -> Proto { fatalError() }

@inline(never)
func allProto<T:Proto>(_ x: __owned T) {
  let res = x.mutatingMethod()
}

@inline(never)
func allInoutProto<T:Proto>(_ x: inout T) { fatalError() }

@inline(never)
func blackHole<T>(_ x: T) { fatalError() }

struct Chonk1 {
  var field0: Double = 0
  var field1: Double = 0
  var field2: Double = 0
  var field3: Double = 0
  var field4: Double = 0
  var field5: Double = 0
  var field6: Double = 0
  var field7: Double = 0
}

struct Chonk4 {
  var field0 = Chonk1() 
  var field1 = Chonk1()
  var field2 = Chonk1()
  var field3 = Chonk1()

  @inline(never)
  func method() {
    blackHole(self.field3)
  }
}

Chonk4().method()


var foo = getProto()
allInoutProto(&foo)

allProto(getProto())

addressorTest()
