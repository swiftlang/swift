// RUN: %target-swift-frontend %s -emit-sil \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

struct View : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  @_lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  @_lifetime(copy otherBV)
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
  @_lifetime(copy k)
  init(consumingView k: consuming View) {
    self.ptr = k.ptr
    self.c = k.c
  }
}

struct Wrapper : ~Escapable {
  var _view: View

  // Nested coroutine access.
  var view: View {
    @_lifetime(copy self)
    _read {
      yield _view
    }
    @_lifetime(&self)
    _modify {
      yield &_view
    }
  }
  @_lifetime(copy view)
  init(_ view: consuming View) {
    self._view = view
  }
}

struct NCContainer : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int

  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }

  // Nested coroutine access.
  var view: View {
    _read {
      yield View(self)
    }
  }

  // Doubly nested coroutine access.
  var wrapper: Wrapper {
    _read {
      yield Wrapper(view)
    }
  }
}

func use(_ o : borrowing View) {}

// Extend access scopes across chained coroutines.
//
// CHECK-LABEL: sil hidden @$s9coroutine20testDoubleNestedRead2ncyAA11NCContainerVn_tF : $@convention(thin) (@owned NCContainer) -> () {
// CHECK: bb0(%0 : $NCContainer):
// CHECK:   [[NC:%.*]] = alloc_stack [lexical] [var_decl] $NCContainer, var, name "nc", type $NCContainer
// CHECK:   store %0 to [[NC]]
//       let wrapper = nc.wrapper
// CHECK:   [[ACCESS:%.*]] = begin_access [read] [static] [[NC]]
// CHECK:   [[NCVAL:%.*]] = load [[ACCESS]] 
// CHECK:   ([[WRAPPER:%.*]], [[TOKEN1:%.*]]) = begin_apply %{{.*}}([[NCVAL]]) : $@yield_once @convention(method) (@guaranteed NCContainer) -> @lifetime(borrow 0) @yields @guaranteed Wrapper
// CHECK:   [[MDI1:%.*]] = mark_dependence [nonescaping] [[WRAPPER]] on [[TOKEN1]]
// CHECK:   [[MDI2:%.*]] = mark_dependence [nonescaping] [[MDI1]] on [[ACCESS]]
// CHECK:   retain_value [[MDI2]]
// CHECK:   debug_value [[MDI2]], let, name "wrapper"
//       let view = wrapper.view
// CHECK:   ([[VIEW:%.*]], [[TOKEN2:%.*]]) = begin_apply %{{.*}}([[MDI2]]) : $@yield_once @convention(method) (@guaranteed Wrapper) -> @lifetime(copy 0) @yields @guaranteed View
// CHECK:   retain_value [[VIEW]]
// CHECK:   end_apply [[TOKEN2]] as $()
// CHECK:   debug_value [[VIEW]], let, name "view"
//       use(view)
// CHECK:   apply %{{.*}}([[VIEW]]) : $@convention(thin) (@guaranteed View) -> ()
// CHECK:   release_value [[VIEW]]
// CHECK:   release_value [[MDI2]]
// CHECK:   end_apply [[TOKEN1]] as $()
// CHECK:   end_access [[ACCESS]]
// CHECK:   destroy_addr [[NC]]
// CHECK-LABEL: } // end sil function '$s9coroutine20testDoubleNestedRead2ncyAA11NCContainerVn_tF'
func testDoubleNestedRead(nc: consuming NCContainer) {
  let wrapper = nc.wrapper
  let view = wrapper.view
  use(view)
}
