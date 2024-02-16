// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify \
// RUN: -enable-builtin-module \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN: -disable-experimental-parser-round-trip \
// RUN: -enable-experimental-feature NoncopyableGenerics \
// RUN: -enable-experimental-lifetime-dependence-inference \
// RUN:  -Xllvm -enable-lifetime-dependence-diagnostics=true

// REQUIRES: asserts

// REQUIRES: swift_in_compiler

struct NCContainer : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct NEContainer : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
}

struct View : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  init(_ otherBV: borrowing View) {
    self.ptr = otherBV.ptr
  }
  init(_ k: borrowing NCContainer) {
    self.ptr = k.ptr
  }
  init(_ k: consuming NEContainer) {
    self.ptr = k.ptr
  }
}

struct MutableView : ~Copyable, ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_unsafeNonescapableResult
  init(_ ptr: UnsafeRawBufferPointer) {
    self.ptr = ptr
  }
  init(_ otherBV: borrowing View) {
    self.ptr = otherBV.ptr
  }
  init(_ k: borrowing NCContainer) {
    self.ptr = k.ptr
  }
  init(_ k: consuming NEContainer) {
    self.ptr = k.ptr
  }
}

func use(_ o : borrowing View) {}
func mutate(_ x: inout NCContainer) { }
func mutate(_ x: inout View) { }
func mutate(_ x: inout NEContainer) { }
func consume(_ o : consuming View) {}
func use(_ o : borrowing MutableView) {}
func consume(_ o : consuming MutableView) {}

func getConsumingView(_ x: consuming NEContainer) -> _consume(x) View {
  return View(x)
}

func getConsumingView(_ x: consuming View) -> _consume(x) View {
  return View(x.ptr)
}

func getBorrowingView(_ x: borrowing View) -> _borrow(x) View {
  return View(x.ptr)
}

func getBorrowingView(_ x: borrowing NCContainer) -> _borrow(x) View {
  return View(x.ptr)
}

func getBorrowingView(_ x: borrowing NEContainer) -> _borrow(x) View {
  return View(x.ptr)
}

func test1(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NEContainer($0)
    mutate(&x)
    let view = getConsumingView(x)
    let newView = View(view)
    use(newView)
    consume(view)
  }
}

func test2(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NCContainer($0)
    mutate(&x)
    let view = getBorrowingView(x)
    use(view)
    consume(view)
  }
}

func test3(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = View($0)
    mutate(&x)
    let view = getConsumingView(x)
    use(view)
    consume(view)
  }
}

/*
// Currently fails because the lifetime dependence util isn't analyzing a
// def-use chain involving a stack temporary
func test4(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NCContainer($0)
    mutate(&x)
    let view = MutableView(x)
    use(view)
    consume(view)
  }
}
*/

func test5(_ a: Array<Int>) {
  a.withUnsafeBytes {
    let x = NEContainer($0)
    let view = getBorrowingView(x)
    let anotherView = getConsumingView(view)
    use(anotherView)
  }
}

func test6(_ a: Array<Int>) {
  var p : View?
  a.withUnsafeBytes {
    var x = NCContainer($0)
    mutate(&x)
    let view = View(x)
    p = view
  }
  use(p!)
}

func test7(_ a: UnsafeRawBufferPointer) {
  var x = NEContainer(a)
  do {
    let view = getBorrowingView(x)
    use(view)
  }
  mutate(&x)
}

func test8(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NEContainer($0)
    mutate(&x)
    let view = MutableView(x)
    use(view)
    consume(view)
  }
}
