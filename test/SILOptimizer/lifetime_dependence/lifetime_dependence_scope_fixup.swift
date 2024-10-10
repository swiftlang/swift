// RUN: %target-swift-frontend %s -emit-sil \
// RUN: -enable-experimental-feature NonescapableTypes \
// RUN:   -disable-experimental-parser-round-trip \
// RUN: | %FileCheck %s
// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

struct NCContainer : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

struct View : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  init(_ otherBV: borrowing View) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
  init(_ k: borrowing NCContainer) {
    self.ptr = k.ptr
    self.c = k.c
  }
  init(_ k: consuming View) {
    self.ptr = k.ptr
    self.c = k.c
  }
}

struct MutableView : ~Copyable, ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  init(_ otherBV: borrowing View) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
  init(_ k: borrowing NCContainer) {
    self.ptr = k.ptr
    self.c = k.c
  }
}

func use(_ o : borrowing View) {}
func mutate(_ x: inout NCContainer) { }
func mutate(_ x: inout View) { }
func consume(_ o : consuming View) {}
func use(_ o : borrowing MutableView) {}
func consume(_ o : consuming MutableView) {}

@lifetime(x)
func getConsumingView(_ x: consuming View) -> View {
  return View(x)
}

@lifetime(borrow x)
func getBorrowingView(_ x: borrowing View) -> View {
  return View(x.ptr, x.c)
}

@lifetime(borrow x)
func getBorrowingView(_ x: borrowing NCContainer) -> View {
  return View(x.ptr, x.c)
}

func test1(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = View($0, a.count)
    mutate(&x)
    let view = getConsumingView(x)
    let newView = View(view)
    use(newView)
    consume(view)
  }
}

// CHECK-LABEL: sil private @$s31lifetime_dependence_scope_fixup5test2yySaySiGFySWXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeRawBufferPointer, @guaranteed Array<Int>) -> (@out τ_0_0, @error any Error) for <()> {
// CHECK:   [[CONT:%.*]] = alloc_stack [lexical] [var_decl] $NCContainer, var, name "x"
// CHECK:   [[BA:%.*]] = begin_access [read] [static] [[CONT]] : $*NCContainer
// CHECK:   [[LD:%.*]] = load [[BA]] : $*NCContainer
// CHECK:   [[FUNC:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup16getBorrowingViewyAA0G0VAA11NCContainerVF : $@convention(thin) (@guaranteed NCContainer) -> @lifetime(borrow 0) @owned View
// CHECK:   [[VIEW:%.*]] = apply [[FUNC]]([[LD]]) : $@convention(thin) (@guaranteed NCContainer) -> @lifetime(borrow 0) @owned View
// CHECK:   [[MDI:%.*]] = mark_dependence [nonescaping] [[VIEW]] : $View on [[BA]] : $*NCContainer
// CHECK:   [[USE:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup3useyyAA4ViewVF : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   apply [[USE]]([[MDI]]) : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   [[CONSUME:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup7consumeyyAA4ViewVnF : $@convention(thin) (@owned View) -> ()
// CHECK:   apply [[CONSUME]]([[MDI]]) : $@convention(thin) (@owned View) -> ()
// CHECK:   end_access [[BA]] : $*NCContainer
// CHECK-LABEL: } // end sil function '$s31lifetime_dependence_scope_fixup5test2yySaySiGFySWXEfU_'
func test2(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NCContainer($0, a.count)
    mutate(&x)
    let view = getBorrowingView(x)
    use(view)
    consume(view)
  }
}

func test3(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = View($0, a.count)
    mutate(&x)
    let view = getConsumingView(x)
    use(view)
    consume(view)
  }
}

func test4(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = NCContainer($0, a.count)
    mutate(&x)
    let view = MutableView(x)
    use(view)
    consume(view)
  }
}

func test5(_ a: Array<Int>) {
  a.withUnsafeBytes {
    let x = View($0, a.count)
    let view = getBorrowingView(x)
    let anotherView = getConsumingView(view)
    use(anotherView)
  }
}

/* Enable once Optional is ~Escapable
func test6(_ a: Array<Int>) {
  var p : View? // error: type 'View' does not conform to protocol 'Escapable'
  a.withUnsafeBytes {
    var x = NCContainer($0, a.count)
    mutate(&x)
    let view = View(x)
    p = view
  }
  use(p!)
}
*/

// CHECK-LABEL: sil hidden @$s31lifetime_dependence_scope_fixup5test7yySWF : $@convention(thin) (UnsafeRawBufferPointer) -> () {
// CHECK:   [[CONT:%.*]] = alloc_stack [var_decl] $View
// CHECK:   [[BA:%.*]] = begin_access [read] [static] [[CONT]] : $*View
// CHECK:   [[LD:%.*]] = load [[BA]] : $*View
// CHECK:   [[FUNC:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup16getBorrowingViewyAA0G0VADF : $@convention(thin) (@guaranteed View) -> @lifetime(borrow 0) @owned View
// CHECK:   [[VIEW:%.*]] = apply [[FUNC]]([[LD]]) : $@convention(thin) (@guaranteed View) -> @lifetime(borrow 0) @owned View
// CHECK:   [[MDI:%.*]] = mark_dependence [nonescaping] [[VIEW]] : $View on [[BA]] : $*View
// CHECK:   [[USE:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup3useyyAA4ViewVF : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   apply [[USE]]([[MDI]]) : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   release_value [[MDI]] : $View
// CHECK:   end_access [[BA]] : $*View
// CHECK-LABEL: } // end sil function '$s31lifetime_dependence_scope_fixup5test7yySWF'
func test7(_ a: UnsafeRawBufferPointer) {
  var x = View(a, a.count)
  do {
    let view = getBorrowingView(x)
    use(view)
  }
  mutate(&x)
}

func test8(_ a: Array<Int>) {
  a.withUnsafeBytes {
    var x = View($0, a.count)
    mutate(&x)
    let view = MutableView(x)
    use(view)
    consume(view)
  }
}

struct Wrapper : ~Escapable {
  var _view: View
  var view: View {
    _read {
      yield _view
    }
    _modify {
      yield &_view
    }
  }
  init(_ view: consuming View) {
    self._view = view
  }
}

func test9() {
  let a = [Int](repeating: 0, count: 4)
  a.withUnsafeBytes {
    let view = View($0, a.count)
    var c = Wrapper(view)
    use(c.view)
    mutate(&c.view)
  }
}

func getViewTuple(_ x: borrowing View) -> (View, View) {
  return (View(x.ptr, x.c), View(x.ptr, x.c))
}

public func test10() {
  let a = [Int](repeating: 0, count: 4)
  a.withUnsafeBytes {
    var x = View($0, a.count)
    mutate(&x)
    let view = getBorrowingView(x)
    let tuple = getViewTuple(view)
    use(tuple.0)
    use(tuple.1)
  }
}

