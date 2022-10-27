// RUN: %target-swift-emit-silgen -enable-experimental-move-only %s | %FileCheck -check-prefix=SILGEN %s
// RUN: %target-swift-emit-sil -enable-experimental-move-only %s | %FileCheck -check-prefix=SIL %s

// Test that makes sure that throughout the pipeline we properly handle
// conditional releases for trivial and non-trivial move only types.

//////////////////////
// Misc Declaration //
//////////////////////

class Klass {}

/////////////////////////
// Struct Declarations //
/////////////////////////

@_moveOnly
struct KlassPairWithoutDeinit {
    var lhs = Klass()
    var rhs = Klass()
}

@_moveOnly
struct KlassPairWithDeinit {
    var lhs = Klass()
    var rhs = Klass()

    deinit {
        print("123")
    }
}

@_moveOnly
struct IntPairWithoutDeinit {
    var k: Int = 5
    var k2: Int = 6
}

@_moveOnly
struct IntPairWithDeinit {
    var k: Int = 5
    var k2: Int = 6

    deinit {
        print("123")
    }
}

func consumeIntPairWithoutDeinit(_ x: __owned IntPairWithoutDeinit) { }
func consumeIntPairWithDeinit(_ x: __owned IntPairWithDeinit) { }
func consumeKlassPairWithoutDeinit(_ x: __owned KlassPairWithoutDeinit) { }
func consumeKlassPairWithDeinit(_ x: __owned KlassPairWithDeinit) { }

var value: Bool { false }

//////////////////
// Struct Tests //
//////////////////

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits24testIntPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits24testIntPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits24testIntPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits27consumeIntPairWithoutDeinityyAA0defG0VnF : $@convention(thin) (@owned IntPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   release_value [[VALUE]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits24testIntPairWithoutDeinityyF'
public func testIntPairWithoutDeinit() {
    let f = IntPairWithoutDeinit()
    if value {
        consumeIntPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits21testIntPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits21testIntPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits21testIntPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits24consumeIntPairWithDeinityyAA0defG0VnF : $@convention(thin) (@owned IntPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL: [[DEINIT:%.*]] = function_ref @$s16moveonly_deinits17IntPairWithDeinitVfD : $@convention(method) (@owned IntPairWithDeinit) -> ()
// SIL: apply [[DEINIT]]([[VALUE]]) : $@convention(method) (@owned IntPairWithDeinit) -> ()
// SIL-NOT: apply
// SIL-NOT: release_value
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits21testIntPairWithDeinityyF'
public func testIntPairWithDeinit() {
    let f = IntPairWithDeinit()
    if value {
        consumeIntPairWithDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits26testKlassPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits26testKlassPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits26testKlassPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits29consumeKlassPairWithoutDeinityyAA0defG0VnF : $@convention(thin) (@owned KlassPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   release_value [[VALUE]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits26testKlassPairWithoutDeinityyF'
public func testKlassPairWithoutDeinit() {
    let f = KlassPairWithoutDeinit()
    if value {
        consumeKlassPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits23testKlassPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits23testKlassPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits23testKlassPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits26consumeKlassPairWithDeinityyAA0defG0VnF : $@convention(thin) (@owned KlassPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL: [[DEINIT:%.*]] = function_ref @$s16moveonly_deinits19KlassPairWithDeinitVfD : $@convention(method) (@owned KlassPairWithDeinit) -> ()
// SIL: apply [[DEINIT]]([[VALUE]]) : $@convention(method) (@owned KlassPairWithDeinit) -> ()
// SIL-NOT: apply
// SIL-NOT: release_value
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits23testKlassPairWithDeinityyF'
public func testKlassPairWithDeinit() {
    let f = KlassPairWithDeinit()
    if value {
        consumeKlassPairWithDeinit(f)
    }
}

///////////////////////
// Enum Declarations //
///////////////////////

@_moveOnly
enum KlassEnumPairWithoutDeinit {
    case lhs(Klass)
    case rhs(Klass)
}

@_moveOnly
enum KlassEnumPairWithDeinit {
    case lhs(Klass)
    case rhs(Klass)

    deinit {
        print("123")
    }
}

@_moveOnly
enum IntEnumPairWithoutDeinit {
case lhs(Int)
case rhs(Int)
}

@_moveOnly
enum IntEnumPairWithDeinit {
    case lhs(Int)
    case rhs(Int)

    deinit {
        print("123")
    }
}

func consumeIntEnumPairWithoutDeinit(_ x: __owned IntEnumPairWithoutDeinit) { }
func consumeIntEnumPairWithDeinit(_ x: __owned IntEnumPairWithDeinit) { }
func consumeKlassEnumPairWithoutDeinit(_ x: __owned KlassEnumPairWithoutDeinit) { }
func consumeKlassEnumPairWithDeinit(_ x: __owned KlassEnumPairWithDeinit) { }

////////////////
// Enum Tests //
////////////////

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits31consumeIntEnumPairWithoutDeinityyAA0defgH0OnF : $@convention(thin) (@owned IntEnumPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   release_value [[VALUE]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF'
public func testIntEnumPairWithoutDeinit() {
    let f = IntEnumPairWithoutDeinit.lhs(5)
    if value {
        consumeIntEnumPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits25testIntEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits25testIntEnumPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits25testIntEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits28consumeIntEnumPairWithDeinityyAA0defgH0OnF : $@convention(thin) (@owned IntEnumPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL: [[DEINIT:%.*]] = function_ref @$s16moveonly_deinits21IntEnumPairWithDeinitOfD : $@convention(method) (@owned IntEnumPairWithDeinit) -> ()
// SIL: apply [[DEINIT]]([[VALUE]]) : $@convention(method) (@owned IntEnumPairWithDeinit) -> ()
// SIL-NOT: apply
// SIL-NOT: release_value
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits25testIntEnumPairWithDeinityyF'
public func testIntEnumPairWithDeinit() {
    let f = IntEnumPairWithDeinit.rhs(6)
    if value {
        consumeIntEnumPairWithDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits33consumeKlassEnumPairWithoutDeinityyAA0defgH0OnF : $@convention(thin) (@owned KlassEnumPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   release_value [[VALUE]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF'
public func testKlassEnumPairWithoutDeinit() {
    let f = KlassEnumPairWithoutDeinit.lhs(Klass())
    if value {
        consumeKlassEnumPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits27testKlassEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[VALUE:%.*]] = mark_must_check [no_implicit_copy] {{%.*}}
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[VALUE]]
// SILGEN: } // end sil function '$s16moveonly_deinits27testKlassEnumPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits27testKlassEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[VALUE:%.*]] = move_value [lexical]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits30consumeKlassEnumPairWithDeinityyAA0defgH0OnF : $@convention(thin) (@owned KlassEnumPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: release_value
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL: [[DEINIT:%.*]] = function_ref @$s16moveonly_deinits23KlassEnumPairWithDeinitOfD : $@convention(method) (@owned KlassEnumPairWithDeinit) -> ()
// SIL: apply [[DEINIT]]([[VALUE]]) : $@convention(method) (@owned KlassEnumPairWithDeinit) -> ()
// SIL-NOT: apply
// SIL-NOT: release_value
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: release_value
// SIL: } // end sil function '$s16moveonly_deinits27testKlassEnumPairWithDeinityyF'
public func testKlassEnumPairWithDeinit() {
    let f = KlassEnumPairWithDeinit.rhs(Klass())
    if value {
        consumeKlassEnumPairWithDeinit(f)
    }
}
