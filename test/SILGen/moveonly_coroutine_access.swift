// RUN: %target-swift-emit-silgen -DTRIVIAL %s | %FileCheck --check-prefix TRIVIAL %s
// RUN: %target-swift-emit-sil -DTRIVIAL -sil-verify-all %s > /dev/null

// RUN: %target-swift-emit-silgen -DLOADABLE %s | %FileCheck --check-prefix LOADABLE %s
// RUN: %target-swift-emit-sil -DLOADABLE -sil-verify-all %s > /dev/null

// RUN: %target-swift-emit-silgen -DADDRESS_ONLY %s | %FileCheck --check-prefix ADDRESS_ONLY %s
// RUN: %target-swift-emit-sil -DADDRESS_ONLY -sil-verify-all %s > /dev/null

class X {
  let snc = SNC()
}

struct NC: ~Copyable {
#if TRIVIAL
  typealias T = Int
  var x: T = 0
#elseif LOADABLE
  typealias T = X
  var x: T = X()
#elseif ADDRESS_ONLY
  typealias T = Any
  var x: T = X()
#else
#error("pick a mode")
#endif

  var consumingGetter: T {
    consuming get { fatalError() }
  }
  var readCoroutine: T {
    _read { yield x }
  }
  consuming func consumingFunc() -> T { fatalError() }
  borrowing func borrowingFunc() -> T { fatalError() }

  deinit { print("destroy") }
}

struct SNC: ~Copyable {
  private var _data: NC = NC()

  subscript(index: Int) -> NC {
    _read { yield _data }
    _modify { yield &_data }
  }
}

func use(_ x: NC.T) {}


// TRIVIAL-LABEL: sil hidden [ossa] @${{.*}}test_subscript_read3sncyAA3SNCV_tF : $@convention(thin) (@guaranteed SNC) -> () {
// TRIVIAL:         ([[YIELDED:%.*]], [[HANDLE:%.*]]) = begin_apply
// TRIVIAL:          [[COPY:%.*]] = copy_value [[YIELDED]]
// TRIVIAL:          [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
// TRIVIAL:          [[BORROW_MARK:%.*]] = begin_borrow [[MARK]]
// TRIVIAL:          [[X:%.*]] = struct_extract [[BORROW_MARK]], #NC.x
// TRIVIAL:          end_borrow [[BORROW_MARK]]
// TRIVIAL:          destroy_value [[MARK]]
// TRIVIAL:          = end_apply [[HANDLE]] as $()
// TRIVIAL:          = apply {{%.*}}([[X]])


// LOADABLE-LABEL: sil hidden [ossa] @${{.*}}test_subscript_read3sncyAA3SNCV_tF : $@convention(thin) (@guaranteed SNC) -> () {
// LOADABLE:         ([[YIELDED:%.*]], [[HANDLE:%.*]]) = begin_apply
// LOADABLE:          [[COPY:%.*]] = copy_value [[YIELDED]]
// LOADABLE:          [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
// LOADABLE:          [[BORROW_MARK:%.*]] = begin_borrow [[MARK]]
// LOADABLE:          [[X:%.*]] = struct_extract [[BORROW_MARK]], #NC.x

// LOADABLE:          [[X_COPY:%.*]] = copy_value [[X]]

// LOADABLE:          end_borrow [[BORROW_MARK]]
// LOADABLE:          destroy_value [[MARK]]
// LOADABLE:          = end_apply [[HANDLE]] as $()

// LOADABLE:          = apply {{%.*}}([[X_COPY]])



// ADDRESS_ONLY-LABEL: sil hidden [ossa] @${{.*}}test_subscript_read3sncyAA3SNCV_tF : $@convention(thin) (@in_guaranteed SNC) -> () {
// ADDRESS_ONLY:         ([[YIELDED:%.*]], [[HANDLE:%.*]]) = begin_apply

// ADDRESS_ONLY:          [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[YIELDED]]
// ADDRESS_ONLY:          [[X_FIELD_ADDR:%.*]] = struct_element_addr [[MARK]], #NC.x
// ADDRESS_ONLY:          [[X_LOCAL_ADDR:%.*]] = alloc_stack $Any
// ADDRESS_ONLY:          copy_addr [[X_FIELD_ADDR]] to [init] [[X_LOCAL_ADDR]]
// ADDRESS_ONLY:          = end_apply [[HANDLE]] as $()
// ADDRESS_ONLY:          = apply {{%.*}}([[X_LOCAL_ADDR]])
// ADDRESS_ONLY:          destroy_addr [[X_LOCAL_ADDR]]
// ADDRESS_ONLY:          dealloc_stack [[X_LOCAL_ADDR]]

func test_subscript_read(snc: borrowing SNC) {
  use(snc[0].x)
}


struct SNC_OPT: ~Copyable {
  private var _data: NC? = NC()

  subscript(index: Int) -> NC? {
    _read { yield _data }
    _modify { yield &_data }
  }
}

// LOADABLE-LABEL:  sil hidden [ossa] @${{.*}}test_subscript_write3sncyAA7SNC_OPTVz_tF

// For the snc[0].take(), ensure the end_apply happens after the call to take.
// LOADABLE:          begin_access [modify]
// LOADABLE:          begin_apply
// LOADABLE:          apply
// LOADABLE:          end_apply
// LOADABLE:          end_access

// LOADABLE:          begin_access [modify]
// LOADABLE:          begin_apply
// LOADABLE:          assign
// LOADABLE:          end_apply
// LOADABLE:          end_access
func test_subscript_write(snc: inout SNC_OPT) {
  let x = snc[0].take()
  snc[0] = x
}
