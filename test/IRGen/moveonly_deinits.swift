// RUN: %target-swift-emit-ir -enable-experimental-move-only %s | %FileCheck -check-prefix=IR %s

// Test that makes sure that at IRGen time we properly handle conditional
// releases for trivial and non-trivial move only types. The SIL/SILGen part of
// this test is in test/SILGen/moveonly_deinits. We have a separate test so that
// we can test on other platforms the other behavior.

// REQUIRES: asserts
// REQUIRES: CPU=x86_64

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

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits24testIntPairWithoutDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:   call swiftcc void @"$s16moveonly_deinits27consumeIntPairWithoutDeinityyAA0defG0VnF"
// IR-NEXT:   br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:   br label %[[CONT]]
//
// IR: [[CONT]]:
// IR-NEXT: ret void
// IR-NEXT: }
public func testIntPairWithoutDeinit() {
    let f = IntPairWithoutDeinit()
    if value {
        consumeIntPairWithoutDeinit(f)
    }
}

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits21testIntPairWithDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits24consumeIntPairWithDeinityyAA0defG0VnF"(
// IR-NEXT:  br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits17IntPairWithDeinitVfD"(
// IR-NEXT:  br label %[[CONT]]
//
// IR: [[CONT]]
// IR-NEXT: ret void
// IR-NEXT: }
public func testIntPairWithDeinit() {
    let f = IntPairWithDeinit()
    if value {
        consumeIntPairWithDeinit(f)
    }
}

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits26testKlassPairWithoutDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:   call swiftcc void @"$s16moveonly_deinits29consumeKlassPairWithoutDeinityyAA0defG0VnF"
// IR-NEXT:   br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:   call void bitcast {{.*}} @swift_release
// IR-NEXT:   call void bitcast {{.*}} @swift_release
// IR-NEXT:   br label %[[CONT]]
//
// IR: [[CONT]]:
// IR-NEXT: ret void
// IR-NEXT: }
public func testKlassPairWithoutDeinit() {
    let f = KlassPairWithoutDeinit()
    if value {
        consumeKlassPairWithoutDeinit(f)
    }
}

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits23testKlassPairWithDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits26consumeKlassPairWithDeinityyAA0defG0VnF"(
// IR-NEXT:  br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits19KlassPairWithDeinitVfD"(
// IR-NEXT:  br label %[[CONT]]
//
// IR: [[CONT]]
// IR-NEXT: ret void
// IR-NEXT: }
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

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:   call swiftcc void @"$s16moveonly_deinits31consumeIntEnumPairWithoutDeinityyAA0defgH0OnF"
// IR-NEXT:   br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:   br label %[[CONT]]
//
// IR: [[CONT]]:
// IR-NEXT: ret void
// IR-NEXT: }
public func testIntEnumPairWithoutDeinit() {
    let f = IntEnumPairWithoutDeinit.lhs(5)
    if value {
        consumeIntEnumPairWithoutDeinit(f)
    }
}

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits25testIntEnumPairWithDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits28consumeIntEnumPairWithDeinityyAA0defgH0OnF"(
// IR-NEXT:  br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits21IntEnumPairWithDeinitOfD"(
// IR-NEXT:  br label %[[CONT]]
//
// IR: [[CONT]]
// IR-NEXT: ret void
// IR-NEXT: }
public func testIntEnumPairWithDeinit() {
    let f = IntEnumPairWithDeinit.rhs(6)
    if value {
        consumeIntEnumPairWithDeinit(f)
    }
}

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:   call swiftcc void @"$s16moveonly_deinits33consumeKlassEnumPairWithoutDeinityyAA0defgH0OnF"
// IR-NEXT:   br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:   and i64
// IR-NEXT:   inttoptr i64
// IR-NEXT:   call void{{.*}} @swift_release
// IR-NEXT:   br label %[[CONT]]
//
// IR: [[CONT]]:
// IR-NEXT: ret void
// IR-NEXT: }
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

// IR-LABEL: define {{.*}}swiftcc void @"$s16moveonly_deinits27testKlassEnumPairWithDeinityyF"()
// IR: br i1 {{%.*}}, label %[[BB1:[0-9]+]], label %[[BB2:[0-9]+]]
//
// IR: [[BB1]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits30consumeKlassEnumPairWithDeinityyAA0defgH0OnF"(
// IR-NEXT:  br label %[[CONT:[0-9]+]]
//
// IR: [[BB2]]:
// IR-NEXT:  call swiftcc void @"$s16moveonly_deinits23KlassEnumPairWithDeinitOfD"(
// IR-NEXT:  br label %[[CONT]]
//
// IR: [[CONT]]
// IR-NEXT: ret void
// IR-NEXT: }
public func testKlassEnumPairWithDeinit() {
    let f = KlassEnumPairWithDeinit.rhs(Klass())
    if value {
        consumeKlassEnumPairWithDeinit(f)
    }
}
