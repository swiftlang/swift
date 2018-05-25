// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s

struct Butt {
  subscript(butts: Int...) -> Int {
    return 0
  }
}

_ = Butt()[1]

struct A {
	subscript(indices: Int...) -> Int {
		get { return 0 }
		set {}
	}
}

func testSetVariadicSubscriptNone() {
	var a = A()
	a[] = 1
}


func testSetVariadicSubscriptSingle() {
	var a = A()
	a[1] = 1
}

func testSetVariadicSubscriptMultiple() {
	var a = A()
	a[1,2,3] = 1
}

struct B {
	subscript(indices: (Int, Int)...) -> Int {
		get { return 0 }
		set {}
	}
}

func testSetVariadicTupleSubscriptNone() {
	var b = B()
	b[] = 1
}


func testSetVariadicTupleSubscriptSingle() {
	var b = B()
	b[(1,2)] = 1
}

func testSetVariadicTupleSubscriptMultiple() {
	var b = B()
	b[(1,2),(2,3)] = 1
}
