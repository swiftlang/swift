struct Point {
  var x: Int
  var y: Int
}

struct Rectangle {
  var topLeft: Point
  var bottomRight: Point
}

@dynamicMemberLookup
struct Lens<T> {
  var obj: T
  init(_ obj: T) {
    self.obj = obj
  }

  subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> Lens<U> {
    get { return Lens<U>(obj[keyPath: member]) }
    set { obj[keyPath: member] = newValue.obj }
  }
}

func test(r: Lens<Rectangle>) {
  _ = r.topLeft
  _ = r.bottomRight.y
}

func keyPathTester<V>(keyPath: KeyPath<Lens<Rectangle>, V>) {}

func testKeyPath() {
  keyPathTester(keyPath: \.topLeft)
}

// RUN: %sourcekitd-test -req=sema %s -- %s > %t.response
// RUN: %diff -u %s.response %t.response
