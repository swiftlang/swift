// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -Xllvm -sil-disable-pass=onone-simplification -emit-sil \
// RUN: -enable-experimental-feature LifetimeDependence \
// RUN: | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

/// Unsafely discard any lifetime dependency on the `dependent` argument. Return
/// a value identical to `dependent` that inherits all lifetime dependencies from
/// the `source` argument.
@_unsafeNonescapableResult
@_transparent
@lifetime(copy source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, copying source: borrowing U
) -> T {
  dependent
}

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
  @lifetime(copy otherBV)
  init(_ otherBV: borrowing View) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
  init(_ k: borrowing NCContainer) {
    self.ptr = k.ptr
    self.c = k.c
  }
  // This overload requires a separate label because overloading
  // on borrowing/consuming attributes is not allowed
  @lifetime(copy k)
  init(consumingView k: consuming View) {
    self.ptr = k.ptr
    self.c = k.c
  }
}

struct NCMutableContainer : ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
  init(_ ptr: UnsafeMutableRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

struct MutableView : ~Copyable, ~Escapable {
  let ptr: UnsafeMutableRawBufferPointer
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
  @lifetime(copy otherBV)
  init(_ otherBV: borrowing MutableView) {
    self.ptr = otherBV.ptr
  }
  init(_ k: borrowing NCMutableContainer) {
    self.ptr = k.ptr
  }
}

extension MutableView {
  @lifetime(&self)
  mutating public func update() -> Self {
    return unsafe MutableView(ptr)
  }
}

func use(_ o : borrowing View) {}
func mutate(_ x: inout NCContainer) { }
func mutate(_ x: inout NCMutableContainer) { }
func mutate(_ x: inout View) { }
func consume(_ o : consuming View) {}
func use(_ o : borrowing MutableView) {}
func consume(_ o : consuming MutableView) {}

@lifetime(copy x)
func getConsumingView(_ x: consuming View) -> View {
  return View(consumingView: x)
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

func test4(_ a: inout Array<Int>) {
  a.withUnsafeMutableBytes {
    var x = NCMutableContainer($0, $0.count)
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
// function_ref View.init(_:_:)
// CHECK:   [[VIEW1:%.*]] = apply %{{.*}}(%0, %{{.*}}, %{{.*}}) : $@convention(method) (UnsafeRawBufferPointer, Int, @thin View.Type) -> @lifetime(borrow 0) @owned View
// CHECK:   [[MD1:%.*]] = mark_dependence [nonescaping] %{{.*}} : $View on %0 : $UnsafeRawBufferPointer
// CHECK:   [[BA:%.*]] = begin_access [read] [static] [[CONT]] : $*View
// CHECK:   [[FUNC:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup16getBorrowingViewyAA0G0VADF : $@convention(thin) (@guaranteed View) -> @lifetime(borrow 0) @owned View
// CHECK:   [[VIEW2:%.*]] = apply [[FUNC]]([[MD1]]) : $@convention(thin) (@guaranteed View) -> @lifetime(borrow 0) @owned View
// CHECK:   [[MD2:%.*]] = mark_dependence [nonescaping] [[VIEW2]] : $View on [[BA]] : $*View
// CHECK:   [[USE:%.*]] = function_ref @$s31lifetime_dependence_scope_fixup3useyyAA4ViewVF : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   apply [[USE]]([[MD2]]) : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   release_value [[MD2]] : $View
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

func test8(_ a: inout Array<Int>) {
  a.withUnsafeMutableBytes {
    var x = View(UnsafeRawBufferPointer(start: $0.baseAddress!, count: $0.count), $0.count)
    mutate(&x)
    let view = MutableView($0)
    use(view)
    consume(view)
  }
}

struct Wrapper : ~Escapable {
  var _view: View
  var view: View {
    @lifetime(copy self)
    _read {
      yield _view
    }
    @lifetime(borrow self)
    _modify {
      yield &_view
    }
  }
  @lifetime(copy view)
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

@lifetime(copy x)
func getViewTuple(_ x: borrowing View) -> (View, View) {
  let x1 = View(x.ptr, x.c)
  let x2 = View(x.ptr, x.c)
  return (_overrideLifetime(x1, copying: x), _overrideLifetime(x2, copying: x))
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

// CHECK-LABEL: sil hidden @$s31lifetime_dependence_scope_fixup37testPointeeDependenceOnMutablePointer1pySPys5Int64VG_tF : $@convention(thin) (UnsafePointer<Int64>) -> () {
// CHECK: bb0(%0 : $UnsafePointer<Int64>):
// CHECK:   [[ALLOC:%.*]] = alloc_stack [var_decl] $UnsafePointer<Int64>, var, name "ptr", type $UnsafePointer<Int64>
// CHECK:   mark_dependence [nonescaping] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s31lifetime_dependence_scope_fixup37testPointeeDependenceOnMutablePointer1pySPys5Int64VG_tF'
func testPointeeDependenceOnMutablePointer(p: UnsafePointer<Int64>) {
  var ptr = p
  _ = ptr.pointee
  _ = ptr
}

// CHECK-LABEL: sil hidden @$s31lifetime_dependence_scope_fixup16testReassignment1bySw_tF : $@convention(thin) (UnsafeMutableRawBufferPointer) -> () {
// CHECK: bb0(%0 : $UnsafeMutableRawBufferPointer):
// CHECK:   [[VAR:%.*]] = alloc_stack [lexical] [var_decl] $MutableView, var, name "span", type $MutableView
// CHECK:   apply %{{.*}}(%0, %{{.*}}) : $@convention(method) (UnsafeMutableRawBufferPointer, @thin MutableView.Type) -> @lifetime(borrow 0) @owned MutableView
// CHECK:   [[ACCESS1:%.*]] = begin_access [modify] [static] [[VAR]] : $*MutableView
// CHECK:   apply %{{.*}}(%{{.*}}) : $@convention(method) (@inout MutableView) -> @lifetime(borrow 0) @owned MutableView
// CHECK:   [[LD1:%.*]] = load %{{.*}} : $*MutableView
// CHECK:   apply %{{.*}}([[LD1]]) : $@convention(thin) (@guaranteed MutableView) -> ()
// CHECK:   end_access [[ACCESS1]] : $*MutableView
// CHECK:   [[ACCESS2:%.*]] = begin_access [modify] [static] [[VAR]] : $*MutableView
// CHECK:   apply %{{.*}}(%{{.*}}) : $@convention(method) (@inout MutableView) -> @lifetime(borrow 0) @owned MutableView
// CHECK:   [[LD2:%.*]] = load %{{.*}} : $*MutableView
// CHECK:   apply %{{.*}}([[LD2]]) : $@convention(thin) (@guaranteed MutableView) -> ()
// CHECK:   end_access [[ACCESS2]] : $*MutableView
// CHECK:   destroy_addr [[VAR]] : $*MutableView
// CHECK-LABEL: } // end sil function '$s31lifetime_dependence_scope_fixup16testReassignment1bySw_tF'
func testReassignment(b: UnsafeMutableRawBufferPointer) {
  var span = MutableView(b)

  var sub = span.update()
  use(sub)

  sub = span.update()
  use(sub)
}


// Coroutine nested in a mutable access scope.
//
// rdar://150275147 (Invalid SIL after lifetime dependence fixup involving coroutines)
struct ArrayWrapper {
  private var a: [Int]

  var array: [Int] {
    _read {
      yield a
    }
    _modify {
      yield &a
    }
  }
}

@_silgen_name("gms")
func getMutableSpan(_: inout [Int]) -> MutableSpan<Int>

func testWrite(_ w: inout ArrayWrapper) {
  var span = getMutableSpan(&w.array)
  for i in span.indices {
    span[i] = 0
  }
}
