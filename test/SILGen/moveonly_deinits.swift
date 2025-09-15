// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -enable-experimental-feature MoveOnlyEnumDeinits %s | %FileCheck -check-prefix=SILGEN %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -enable-experimental-feature MoveOnlyEnumDeinits %s | %FileCheck -check-prefix=SIL %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -O -sil-verify-all -enable-experimental-feature MoveOnlyEnumDeinits %s

// REQUIRES: swift_feature_MoveOnlyEnumDeinits

// Test that makes sure that throughout the pipeline we properly handle
// conditional releases for trivial and non-trivial move only types.

////////////////////////////
// MARK: Misc Declaration //
////////////////////////////

class Klass {}

///////////////////////////////
// MARK: Struct Declarations //
///////////////////////////////

@inline(never)
func doSomething() {
    print("123")
}

struct KlassPairWithoutDeinit: ~Copyable {
    var lhs = Klass()
    var rhs = Klass()
}

struct KlassPairWithDeinit: ~Copyable {
    var lhs = Klass()
    var rhs = Klass()

    deinit {
        doSomething()
    }
}

struct IntPairWithoutDeinit: ~Copyable {
    var k: Int = 5
    var k2: Int = 6
}

struct IntPairWithDeinit: ~Copyable {
    var k: Int = 5
    var k2: Int = 6

    deinit {
        doSomething()
    }
}

func consumeIntPairWithoutDeinit(_ x: __owned IntPairWithoutDeinit) { }
func consumeIntPairWithDeinit(_ x: __owned IntPairWithDeinit) { }
func consumeKlassPairWithoutDeinit(_ x: __owned KlassPairWithoutDeinit) { }
func consumeKlassPairWithDeinit(_ x: __owned KlassPairWithDeinit) { }

var value: Bool { false }

//////////////////////////////////////
// MARK: Struct Deinit Output Tests //
//////////////////////////////////////

// SILGEN-LABEL: sil hidden [ossa] @$s16moveonly_deinits19KlassPairWithDeinitVfD : $@convention(method) (@owned KlassPairWithDeinit) -> () {
// SILGEN: bb0([[ARG:%.*]] :
// SILGEN:   [[STACK:%.*]] = alloc_stack
// SILGEN:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
// SILGEN:   store [[ARG]] to [init] [[MARK]]
// SILGEN:   [[DD:%.*]] = drop_deinit [[MARK]]
// SILGEN:   [[L_ADDR:%[^,]+]] = struct_element_addr [[DD]]
// SILGEN-SAME: #KlassPairWithDeinit.lhs 
// SILGEN:   [[L_ACCESS:%[^,]+]] = begin_access [deinit] [static] [[L_ADDR]]
// SILGEN:   destroy_addr [[L_ACCESS]]
// SILGEN:   [[R_ADDR:%[^,]+]] = struct_element_addr [[DD]]
// SILGEN-SAME: #KlassPairWithDeinit.rhs 
// SILGEN:   [[R_ACCESS:%[^,]+]] = begin_access [deinit] [static] [[R_ADDR]]
// SILGEN:   destroy_addr [[R_ACCESS]]
// SILGEN: } // end sil function '$s16moveonly_deinits19KlassPairWithDeinitVfD'

// SILGEN-LABEL: sil hidden [ossa] @$s16moveonly_deinits17IntPairWithDeinitVfD : $@convention(method) (@owned IntPairWithDeinit) -> () {
// SILGEN: bb0([[ARG:%.*]] :
// SILGEN:   [[STACK:%.*]] = alloc_stack
// SILGEN:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
// SILGEN:   store [[ARG]] to [init] [[MARK]]
// SILGEN:   [[DD:%.*]] = drop_deinit [[MARK]]
// SILGEN: } // end sil function '$s16moveonly_deinits17IntPairWithDeinitVfD'

////////////////////////
// MARK: Struct Tests //
////////////////////////

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits24testIntPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// SILGEN: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits24testIntPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits24testIntPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[CONSTRUCTOR:%[^,]+]] = function_ref @$s16moveonly_deinits20IntPairWithoutDeinitVACycfC
// SIL: [[VALUE:%.*]] = apply [[CONSTRUCTOR]]
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[LOADED_VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits27consumeIntPairWithoutDeinityyAA0defG0VnF : $@convention(thin) (@owned IntPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[LOADED_VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   destroy_addr [[STACK]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits24testIntPairWithoutDeinityyF'
public func testIntPairWithoutDeinit() {
    let f = IntPairWithoutDeinit()
    if value {
        consumeIntPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits21testIntPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// SILGEN: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits21testIntPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits21testIntPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[CONSTRUCTOR:%[^,]+]] = function_ref @$s16moveonly_deinits17IntPairWithDeinitVACycfC
// SIL: [[VALUE:%.*]] = apply [[CONSTRUCTOR]]
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits24consumeIntPairWithDeinityyAA0defG0VnF : $@convention(thin) (@owned IntPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL:   destroy_addr [[STACK]] : $*IntPairWithDeinit 
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits21testIntPairWithDeinityyF'
public func testIntPairWithDeinit() {
    let f = IntPairWithDeinit()
    if value {
        consumeIntPairWithDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits26testKlassPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[BORROWED_BOX:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// SILGEN: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits26testKlassPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits26testKlassPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[CONSTRUCTOR:%[^,]+]] = function_ref @$s16moveonly_deinits22KlassPairWithoutDeinitVACycfC
// SIL: [[VALUE:%.*]] = apply [[CONSTRUCTOR]]
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits29consumeKlassPairWithoutDeinityyAA0defG0VnF : $@convention(thin) (@owned KlassPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   destroy_addr [[STACK]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits26testKlassPairWithoutDeinityyF'
public func testKlassPairWithoutDeinit() {
    let f = KlassPairWithoutDeinit()
    if value {
        consumeKlassPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits23testKlassPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits23testKlassPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits23testKlassPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[CONSTRUCTOR:%[^,]+]] = function_ref @$s16moveonly_deinits19KlassPairWithDeinitVACycfC
// SIL: [[VALUE:%.*]] = apply [[CONSTRUCTOR]]
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits26consumeKlassPairWithDeinityyAA0defG0VnF : $@convention(thin) (@owned KlassPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL:   destroy_addr [[STACK]] : $*KlassPairWithDeinit
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits23testKlassPairWithDeinityyF'
public func testKlassPairWithDeinit() {
    let f = KlassPairWithDeinit()
    if value {
        consumeKlassPairWithDeinit(f)
    }
}

/////////////////////////////
// MARK: Enum Declarations //
/////////////////////////////

enum KlassEnumPairWithoutDeinit: ~Copyable {
    case lhs(Klass)
    case rhs(Klass)
}

enum KlassEnumPairWithDeinit: ~Copyable {
    case lhs(Klass)
    case rhs(Klass)

    deinit {
        doSomething()
    }
}

enum IntEnumPairWithoutDeinit: ~Copyable {
case lhs(Int)
case rhs(Int)
}

enum IntEnumPairWithDeinit: ~Copyable {
    case lhs(Int)
    case rhs(Int)

    deinit {
        doSomething()
    }
}

func consumeIntEnumPairWithoutDeinit(_ x: __owned IntEnumPairWithoutDeinit) { }
func consumeIntEnumPairWithDeinit(_ x: __owned IntEnumPairWithDeinit) { }
func consumeKlassEnumPairWithoutDeinit(_ x: __owned KlassEnumPairWithoutDeinit) { }
func consumeKlassEnumPairWithDeinit(_ x: __owned KlassEnumPairWithDeinit) { }

////////////////////////////////////
// MARK: Enum Deinit Output Tests //
////////////////////////////////////

// SILGEN-LABEL: sil hidden [ossa] @$s16moveonly_deinits23KlassEnumPairWithDeinitOfD : $@convention(method) (@owned KlassEnumPairWithDeinit) -> () {
// SILGEN: bb0([[ARG:%.*]] :
// SILGEN:   [[STACK:%.*]] = alloc_stack
// SILGEN:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
// SILGEN:   store [[ARG]] to [init] [[MARK]]
// SILGEN:   [[DD:%.*]] = drop_deinit [[MARK]]
// SILGEN:   switch_enum_addr [[DD]]
// SILGEN-SAME:  case #KlassEnumPairWithDeinit.lhs!enumelt: [[BASIC_BLOCK1:bb[0-9]+]]
// SILGEN-SAME:  case #KlassEnumPairWithDeinit.rhs!enumelt: [[BASIC_BLOCK2:bb[0-9]+]]
// SILGEN: [[BASIC_BLOCK1]]:
// SILGEN:   [[L_ADDR:%[^,]+]] = unchecked_take_enum_data_addr [[DD]]
// SILGEN-SAME:  #KlassEnumPairWithDeinit.lhs!enumelt
// SILGEN:   destroy_addr [[L_ADDR]]
// SILGEN: [[BASIC_BLOCK2]]:
// SILGEN:   [[R_ADDR:%[^,]+]] = unchecked_take_enum_data_addr [[DD]]
// SILGEN-SAME:  #KlassEnumPairWithDeinit.rhs!enumelt
// SILGEN:   destroy_addr [[R_ADDR]]
// SILGEN: } // end sil function '$s16moveonly_deinits23KlassEnumPairWithDeinitOfD'

// SILGEN-LABEL: sil hidden [ossa] @$s16moveonly_deinits21IntEnumPairWithDeinitOfD : $@convention(method) (@owned IntEnumPairWithDeinit) -> () {
// SILGEN: bb0([[ARG:%.*]] :
// SILGEN:   [[STACK:%.*]] = alloc_stack
// SILGEN:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
// SILGEN:   store [[ARG]] to [init] [[MARK]]
// SILGEN:   [[DD:%.*]] = drop_deinit [[MARK]]
// SILGEN:   switch_enum_addr [[DD]]
// SILGEN-SAME:  case #IntEnumPairWithDeinit.lhs!enumelt: [[BASIC_BLOCK1:bb[0-9]+]]
// SILGEN-SAME:  case #IntEnumPairWithDeinit.rhs!enumelt: [[BASIC_BLOCK2:bb[0-9]+]]
// SILGEN: [[BASIC_BLOCK1]]:
// SILGEN:   [[L_ADDR:%[^,]+]] = unchecked_take_enum_data_addr [[DD]]
// SILGEN-SAME:  #IntEnumPairWithDeinit.lhs!enumelt
// SILGEN:   destroy_addr [[L_ADDR]]
// SILGEN: [[BASIC_BLOCK2]]:
// SILGEN:   [[R_ADDR:%[^,]+]] = unchecked_take_enum_data_addr [[DD]]
// SILGEN-SAME:  #IntEnumPairWithDeinit.rhs!enumelt
// SILGEN:   destroy_addr [[R_ADDR]]
// SILGEN: } // end sil function '$s16moveonly_deinits21IntEnumPairWithDeinitOfD'

//////////////////////
// MARK: Enum Tests //
//////////////////////

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// SILGEN: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[VALUE:%.*]] = enum $IntEnumPairWithoutDeinit
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits31consumeIntEnumPairWithoutDeinityyAA0defgH0OnF : $@convention(thin) (@owned IntEnumPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   destroy_addr [[STACK]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits28testIntEnumPairWithoutDeinityyF'
public func testIntEnumPairWithoutDeinit() {
    let f = IntEnumPairWithoutDeinit.lhs(5)
    if value {
        consumeIntEnumPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits25testIntEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[BOX_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[BOX]]
// SILGEN: [[PROJECT:%.*]] = project_box [[BOX_LIFETIME]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits25testIntEnumPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits25testIntEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[VALUE:%.*]] = enum $IntEnumPairWithDeinit
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits28consumeIntEnumPairWithDeinityyAA0defgH0OnF : $@convention(thin) (@owned IntEnumPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL:   destroy_addr [[STACK]] : $*IntEnumPairWithDeinit
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits25testIntEnumPairWithDeinityyF'
public func testIntEnumPairWithDeinit() {
    let f = IntEnumPairWithDeinit.rhs(6)
    if value {
        consumeIntEnumPairWithDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[VALUE:%.*]] = enum $KlassEnumPairWithoutDeinit
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits33consumeKlassEnumPairWithoutDeinityyAA0defgH0OnF : $@convention(thin) (@owned KlassEnumPairWithoutDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL-NOT: apply
// SIL:   destroy_addr [[STACK]]
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits30testKlassEnumPairWithoutDeinityyF'
public func testKlassEnumPairWithoutDeinit() {
    let f = KlassEnumPairWithoutDeinit.lhs(Klass())
    if value {
        consumeKlassEnumPairWithoutDeinit(f)
    }
}

// SILGEN-LABEL: sil [ossa] @$s16moveonly_deinits27testKlassEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SILGEN: [[BOX:%.*]] = alloc_box
// SILGEN: [[PROJECT:%.*]] = project_box [[BORROWED_BOX]]
// SILGEN: cond_br {{%.*}}, bb1, bb2
//
// SILGEN: bb1:
// SILGEN:   [[MARKED:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[PROJECT]]
// SILGEN:   [[LOAD:%.*]] = load [copy] [[MARKED]]
// SILGEN:   apply {{%.*}}([[LOAD]])
// SILGEN:   br bb3
//
// SILGEN: bb2:
// SILGEN:   br bb3
//
// SILGEN: bb3:
// SILGEN:   destroy_value [[BOX]]
// SILGEN: } // end sil function '$s16moveonly_deinits27testKlassEnumPairWithDeinityyF'

// SIL-LABEL: sil @$s16moveonly_deinits27testKlassEnumPairWithDeinityyF : $@convention(thin) () -> () {
// SIL: [[STACK:%.*]] = alloc_stack
// SIL: [[VALUE:%.*]] = enum $KlassEnumPairWithDeinit
// SIL: store [[VALUE]] to [[STACK]]
// SIL: cond_br {{%.*}}, bb1, bb2
//
// SIL: bb1:
// SIL:   [[VALUE:%.*]] = load [[STACK]]
// SIL:   [[FUNC_REF:%.*]] = function_ref @$s16moveonly_deinits30consumeKlassEnumPairWithDeinityyAA0defgH0OnF : $@convention(thin) (@owned KlassEnumPairWithDeinit) -> ()
// SIL:   apply [[FUNC_REF]]([[VALUE]])
// SIL-NOT: destroy_addr
// SIL-NOT: apply
// SIL:   br bb3
//
// SIL: bb2:
// SIL:   destroy_addr [[STACK]] : $*KlassEnumPairWithDeinit
// SIL:   br bb3
//
// SIL: bb3:
// SIL-NOT: destroy_addr
// SIL: } // end sil function '$s16moveonly_deinits27testKlassEnumPairWithDeinityyF'
public func testKlassEnumPairWithDeinit() {
    let f = KlassEnumPairWithDeinit.rhs(Klass())
    if value {
        consumeKlassEnumPairWithDeinit(f)
    }
}
