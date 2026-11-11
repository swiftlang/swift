// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -module-name test -verify %s \
// RUN:   -Xllvm -sil-print-after=raw-sil-inst-lowering -o /dev/null 2>&1 | %FileCheck %s

// For extra coverage, compile without opaque values, but skip FileCheck.
// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -disable-sil-opaque-values -verify %s -o /dev/null

// When a derived class initializer fails, how `self` is cleaned up depends on how far it got. Before
// `super.init`, its initialized stored properties are destroyed and `self` is deallocated without running
// its deinit. A failed `super.init` already released `self`, so nothing is left to clean up. After
// `super.init`, `self` is released as usual.

class Link {}
struct E: Error {}

func mayThrow() throws {}

class Base {
  init() {}
  init(t: Bool) throws {}
  init?(n: Int) {}
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test16ThrowBeforeSuperC1qACSi_tKcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $ThrowBeforeSuper
// CHECK:         try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
// CHECK:       [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK-NEXT:    [[BORROW:%.*]] = load_borrow [[SELF]]
// CHECK-NEXT:    [[X:%.*]] = ref_element_addr [[BORROW]], #ThrowBeforeSuper.x
// CHECK-NEXT:    destroy_addr [[X]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[INSTANCE:%.*]] = load [take] [[SELF]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick ThrowBeforeSuper.Type
// CHECK-NEXT:    dealloc_partial_ref [[INSTANCE]], [[METATYPE]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    throw [[E]]
// CHECK:       } // end sil function '$s4test16ThrowBeforeSuperC1qACSi_tKcfc'
class ThrowBeforeSuper: Base {
  var x: Link
  var y: Link
  init(q: Int) throws {
    x = Link()
    try mayThrow()
    y = Link()
    super.init()
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test13ThrowingSuperC1tACSb_tKcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $ThrowingSuper
// CHECK:         try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
// CHECK:       [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    throw [[E]]
// CHECK:       } // end sil function '$s4test13ThrowingSuperC1tACSb_tKcfc'
class ThrowingSuper: Base {
  var x: Link
  override init(t: Bool) throws {
    x = Link()
    try super.init(t: t)
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test15ThrowAfterSuperC1qACSi_tKcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $ThrowAfterSuper
// CHECK:         try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
// CHECK:       [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK-NEXT:    dealloc_stack [[SELF]]
// CHECK-NEXT:    throw [[E]]
// CHECK:       } // end sil function '$s4test15ThrowAfterSuperC1qACSi_tKcfc'
class ThrowAfterSuper: Base {
  var x: Link
  init(q: Int) throws {
    x = Link()
    super.init()
    try mayThrow()
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test15FailBeforeSuperC4failACSgSb_tcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $FailBeforeSuper
// CHECK:         destroy_addr [[SELF]]
// CHECK:         [[BORROW:%.*]] = load_borrow [[SELF]]
// CHECK-NEXT:    [[X:%.*]] = ref_element_addr [[BORROW]], #FailBeforeSuper.x
// CHECK-NEXT:    destroy_addr [[X]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[INSTANCE:%.*]] = load [take] [[SELF]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick FailBeforeSuper.Type
// CHECK-NEXT:    dealloc_partial_ref [[INSTANCE]], [[METATYPE]]
// CHECK:       } // end sil function '$s4test15FailBeforeSuperC4failACSgSb_tcfc'
class FailBeforeSuper: Base {
  var x: Link
  init?(fail: Bool) {
    x = Link()
    if fail { return nil }
    super.init()
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test14FailAfterSuperC4failACSgSb_tcfc :
// CHECK-NOT:     dealloc_partial_ref
// CHECK:       } // end sil function '$s4test14FailAfterSuperC4failACSgSb_tcfc'
class FailAfterSuper: Base {
  var x: Link
  init?(fail: Bool) {
    x = Link()
    super.init()
    if fail { return nil }
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test13FailableSuperC1nACSgSi_tcfc :
// CHECK-NOT:     dealloc_partial_ref
// CHECK:       } // end sil function '$s4test13FailableSuperC1nACSgSi_tcfc'
class FailableSuper: Base {
  var x: Link
  override init?(n: Int) {
    x = Link()
    super.init(n: n)
  }
}

// Both a failed `super.init`, which released `self`, and a failure after it reach the same `destroy_addr`.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test9FailEmptyC1n4failACSgSi_Sbtcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [dynamic_lifetime] [lexical] [var_decl] $FailEmpty
// CHECK-NOT:     dealloc_partial_ref
// CHECK:         [[HOLDS_REFERENCE:%.*]] = builtin "cmp_ne_Int2"
// CHECK-NEXT:    cond_br [[HOLDS_REFERENCE]], [[HOLDS:bb[0-9]+]], [[EMPTY:bb[0-9]+]]
// CHECK:       [[HOLDS]]:
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK:       [[EMPTY]]:
// CHECK-NEXT:    br
// CHECK-NOT:     dealloc_partial_ref
// CHECK:       } // end sil function '$s4test9FailEmptyC1n4failACSgSi_Sbtcfc'
class FailEmpty: Base {
  init?(n: Int, fail: Bool) {
    super.init(n: n)
    if fail { return nil }
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test7FailOwnC1n4failACSgSi_Sbtcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [dynamic_lifetime] [lexical] [var_decl] $FailOwn
// CHECK-NOT:     dealloc_partial_ref
// CHECK:         [[HOLDS_REFERENCE:%.*]] = builtin "cmp_ne_Int3"
// CHECK-NEXT:    cond_br [[HOLDS_REFERENCE]], [[HOLDS:bb[0-9]+]], [[EMPTY:bb[0-9]+]]
// CHECK:       [[HOLDS]]:
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK:       [[EMPTY]]:
// CHECK-NEXT:    br
// CHECK-NOT:     dealloc_partial_ref
// CHECK:       } // end sil function '$s4test7FailOwnC1n4failACSgSi_Sbtcfc'
class FailOwn: Base {
  var k: Link
  init?(n: Int, fail: Bool) {
    k = Link()
    super.init(n: n)
    if fail { return nil }
  }
}

// A failure before `super.init`, and a failed `super.init`, reach the same `destroy_addr`.
// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test9FailEarlyC1n4failACSgSi_Sbtcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [dynamic_lifetime] [lexical] [var_decl] $FailEarly
// CHECK:         [[HOLDS_REFERENCE:%.*]] = builtin "cmp_ne_Int3"
// CHECK-NEXT:    cond_br [[HOLDS_REFERENCE]], [[HOLDS:bb[0-9]+]], [[EMPTY:bb[0-9]+]]
// CHECK:       [[HOLDS]]:
// CHECK:         [[SUPER_INITIALIZED:%.*]] = builtin "cmp_ne_Int3"
// CHECK-NEXT:    cond_br [[SUPER_INITIALIZED]], [[KEEP:bb[0-9]+]], [[DEALLOC:bb[0-9]+]]
// CHECK:       [[KEEP]]:
// CHECK-NEXT:    destroy_addr [[SELF]]
// CHECK:       [[DEALLOC]]:
// CHECK-NEXT:    [[BORROW:%.*]] = load_borrow [[SELF]]
// CHECK-NEXT:    [[K:%.*]] = ref_element_addr [[BORROW]], #FailEarly.k
// CHECK-NEXT:    destroy_addr [[K]]
// CHECK-NEXT:    end_borrow [[BORROW]]
// CHECK-NEXT:    [[INSTANCE:%.*]] = load [take] [[SELF]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick FailEarly.Type
// CHECK-NEXT:    dealloc_partial_ref [[INSTANCE]], [[METATYPE]]
// CHECK:       [[EMPTY]]:
// CHECK-NEXT:    br
// CHECK:       } // end sil function '$s4test9FailEarlyC1n4failACSgSi_Sbtcfc'
class FailEarly: Base {
  var k: Link
  init?(n: Int, fail: Bool) {
    k = Link()
    if fail { return nil }
    super.init(n: n)
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test10CatchEmptyC1tACSb_tKcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $CatchEmpty
// CHECK:         builtin "willThrow"
// CHECK-NOT:     destroy_addr
// CHECK-NOT:     dealloc_partial_ref
// CHECK:         dealloc_stack [[SELF]]{{ |$}}
// CHECK-NEXT:    throw
// CHECK:       } // end sil function '$s4test10CatchEmptyC1tACSb_tKcfc'
class CatchEmpty: Base {
  override init(t: Bool) throws {
    do {
      try super.init(t: t)
    } catch {
      throw E()
    }
  }
}

// CHECK-LABEL: sil hidden [ossa] [opaque] @$s4test21EmptyThrowBeforeSuperC1qACSi_tKcfc :
// CHECK:         [[SELF:%.*]] = alloc_stack [lexical] [var_decl] $EmptyThrowBeforeSuper
// CHECK:         try_apply {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
// CHECK:       [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK-NEXT:    [[INSTANCE:%.*]] = load [take] [[SELF]]
// CHECK-NEXT:    [[METATYPE:%.*]] = metatype $@thick EmptyThrowBeforeSuper.Type
// CHECK-NEXT:    dealloc_partial_ref [[INSTANCE]], [[METATYPE]]
// CHECK:       } // end sil function '$s4test21EmptyThrowBeforeSuperC1qACSi_tKcfc'
class EmptyThrowBeforeSuper: Base {
  init(q: Int) throws {
    try mayThrow()
    super.init()
  }
}
