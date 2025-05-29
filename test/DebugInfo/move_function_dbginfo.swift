// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -g -emit-ir -o - %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -g -c %s -o %t/out.o
// RUN: %llvm-dwarfdump --show-children %t/out.o | %FileCheck -check-prefix=DWARF %s

// This test checks that:
//
// 1. At the IR level, we insert the appropriate #dbg_addr, #dbg_value.
//
// 2. At the Dwarf that we have proper locations with PC validity ranges where
//    the value isn't valid.

// We only run this on macOS right now since we would need to pattern match
// slightly differently on other platforms.
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64 || CPU=arm64

//////////////////
// Declarations //
//////////////////

public class Klass {
    public func doSomething() {}
}

public protocol P {
    static var value: P { get }
    func doSomething()
}

public var trueValue: Bool { true }
public var falseValue: Bool { false }

///////////
// Tests //
///////////

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo17copyableValueTestyyF"()
//
// In contrast, we should have a dbg.declare for m since we aren't
// CHECK: #dbg_declare(ptr %m.debug, ![[M_COPYABLE_VALUE_TEST:[0-9]*]],
//
// We should have a #dbg_value for k since we moved it.
// CHECK: #dbg_value(ptr %k.debug, ![[K_COPYABLE_VALUE_METADATA:[0-9]*]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// Our undef should be an #dbg_value. Importantly though its metadata
// should be for k since that is the variable that we are telling the debugger
// is no longer defined.
// CHECK: #dbg_value(ptr undef, ![[K_COPYABLE_VALUE_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK-NOT: br label
//
// CHECK: ret void
// CHECK-NEXT: }
// Anchor to the next function in line.
// CHECK: define swiftcc %swift.metadata_response @"$s21move_function_dbginfo5KlassCMa"(i64 %0)
//
// DWARF: DW_AT_linkage_name{{.*}}("$s3out17copyableValueTestyyF")
// DWARF-NEXT: DW_AT_name ("copyableValueTest")
// DWARF-NEXT: DW_AT_decl_file
// DWARF-NEXT: DW_AT_decl_line
// DWARF-NEXT: DW_AT_type
// DWARF-NEXT: DW_AT_external	(true)
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(0x{{[a-z0-9]+}}:
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name	("k")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(
// DWARF-NEXT: DW_AT_name	("m")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
public func copyableValueTest() {
    let k = Klass()
    k.doSomething()
    let m = consume k
    m.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo15copyableArgTestyyAA5KlassCnF"(ptr %0)
//
// In contrast, we should have a dbg.declare for m since we aren't
// CHECK: #dbg_declare(ptr %m.debug, ![[M_COPYABLE_VALUE_TEST:[0-9]*]],
//
// We should have a #dbg_value for k since we moved it.
// CHECK: #dbg_value(ptr %k.debug, ![[K_COPYABLE_VALUE_METADATA:[0-9]*]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// Our undef should be an #dbg_value. Importantly though its metadata
// should be for k since that is the variable that we are telling the debugger
// is no longer defined.
// CHECK: #dbg_value(ptr undef, ![[K_COPYABLE_VALUE_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK-NOT: br label
//
// CHECK: ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name{{.*}}("$s3out15copyableArgTestyyAA5KlassCnF")
// DWARF-NEXT: DW_AT_name ("copyableArgTest")
// DWARF-NEXT: DW_AT_decl_file
// DWARF-NEXT: DW_AT_decl_line
// DWARF-NEXT: DW_AT_type
// DWARF-NEXT: DW_AT_external	(true)
//
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(0x{{[a-z0-9]+}}:
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name	("k")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(
// DWARF-NEXT: DW_AT_name	("m")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
public func copyableArgTest(_ k: __owned Klass) {
    k.doSomething()
    let m = consume k
    m.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo15copyableVarTestyyF"()
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK: #dbg_value(ptr undef, ![[K_COPYABLE_VAR_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK: #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name	("$s3out15copyableVarTestyyF")
// DWARF-NEXT: DW_AT_name	("copyableVarTest")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
// DWARF-NEXT: DW_AT_external	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(0x{{[a-z0-9]+}}:
// We check that we get two separate locations for the different lifetimes of
// the values.
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name	("k")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(
// DWARF-NEXT: DW_AT_name	("m")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(

public func copyableVarTest() {
    var k = Klass()
    k.doSomething()
    let m = consume k
    m.doSomething()
    k = Klass()
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo18copyableVarArgTestyyAA5KlassCzF"(
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK: #dbg_value(ptr undef, ![[K_COPYABLE_VAR_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK: #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name	("$s3out18copyableVarArgTestyyAA5KlassCzF")
// DWARF-NEXT: DW_AT_name	("copyableVarArgTest")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
// DWARF-NEXT: DW_AT_external	(
//
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(0x{{[a-z0-9]+}}:
// We check that we get two separate locations for the different lifetimes of
// the values.
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name	("k")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(
// DWARF-NEXT: DW_AT_name	("m")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
public func copyableVarArgTest(_ k: inout Klass) {
    k.doSomething()
    let m = consume k
    m.doSomething()
    k = Klass()
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo20addressOnlyValueTestyyxAA1PRzlF"(ptr noalias %0, ptr %T, ptr %T.P)
// CHECK: #dbg_value(ptr %{{.*}}, ![[K_ADDR_LET_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK: #dbg_value(ptr undef, ![[K_ADDR_LET_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name   ("$s3out20addressOnlyValueTestyyxAA1PRzlF")
// DWARF-NEXT: DW_AT_name      ("addressOnlyValueTest")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
// DWARF-NEXT: DW_AT_external  (
//
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location  (
// DWARF-NEXT: DW_AT_name      ("x")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (
// DWARF-NEXT: DW_AT_name      ("$\317\204_0_0")
// DWARF-NEXT: DW_AT_type      (
// DWARF-NEXT: DW_AT_artificial        (true)
//
// DWARF: DW_TAG_variable
// DWARF: DW_AT_location    
// DWARF: DW_AT_name    ("$WT\317\204_0_0$$$s3out1P_pD")
// DWARF: DW_AT_type    (
// DWARF: DW_AT_artificial  (true)
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (0x{{[a-z0-9]+}}:
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name      ("k")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
//
public func addressOnlyValueTest<T : P>(_ x: T) {
    let k = x
    k.doSomething()
    let m = consume k
    m.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo23addressOnlyValueArgTestyyxnAA1PRzlF"(
// CHECK: #dbg_declare(ptr %T1,
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr %k.debug, ![[K_ADDR_LET_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK: #dbg_value(ptr undef, ![[K_ADDR_LET_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
public func addressOnlyValueArgTest<T : P>(_ k: __owned T) {
    k.doSomething()
    let m = consume k
    m.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo18addressOnlyVarTestyyxAA1PRzlF"(ptr noalias %0, ptr %T, ptr %T.P)
// CHECK: #dbg_value(ptr %{{.*}}, ![[K_ADDRONLY_VAR_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK-NEXT: br
// CHECK: #dbg_value(ptr undef, ![[K_ADDRONLY_VAR_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK: #dbg_value(ptr %{{.*}}, ![[K_ADDRONLY_VAR_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
public func addressOnlyVarTest<T : P>(_ x: T) {
    var k = x // << this
    k.doSomething()
    let m = consume k
    m.doSomething()
    k = x
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo21addressOnlyVarArgTestyyxz_xtAA1PRzlF"(
// CHECK: #dbg_value(ptr %k.debug, ![[K_ADDRONLY_VAR_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
// CHECK: #dbg_value(ptr undef, ![[K_ADDRONLY_VAR_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: #dbg_value(ptr %k.debug, ![[K_ADDRONLY_VAR_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name   ("$s3out21addressOnlyVarArgTestyyxz_xtAA1PRzlF")
// DWARF-NEXT: DW_AT_name      ("addressOnlyVarArgTest")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
// DWARF-NEXT: DW_AT_external  (
//
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location  (0x{{[a-z0-9]+}}:
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name      ("k")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
//
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location  (
// DWARF-NEXT: DW_AT_name      ("x")
// DWARF-NEXT: DW_AT_decl_file (
// DWARF-NEXT: DW_AT_decl_line (
// DWARF-NEXT: DW_AT_type      (
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (
// DWARF-NEXT: DW_AT_name      ("$\317\204_0_0")
// DWARF-NEXT: DW_AT_type      (
// DWARF-NEXT: DW_AT_artificial        (true)
public func addressOnlyVarArgTest<T : P>(_ k: inout T, _ x: T) {
    k.doSomething()
    let m = consume k
    m.doSomething()
    k = x
    k.doSomething()
}

///////////////////////
// Conditional Tests //
///////////////////////

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo23copyableValueCCFlowTestyyF"(
// CHECK: #dbg_value(ptr %k.debug, ![[K_COPYABLE_LET_CCFLOW_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]+]]
//
// CHECK:    br i1 {{%[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:    #dbg_value(ptr undef, ![[K_COPYABLE_LET_CCFLOW_METADATA]], !DIExpression(), ![[ADDR_LOC]]
public func copyableValueCCFlowTest() {
    let k = Klass()
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo26copyableValueArgCCFlowTestyyAA5KlassCnF"(
// CHECK: #dbg_value(ptr %k.debug, ![[K_COPYABLE_LET_CCFLOW_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]+]]
//
// CHECK:    br i1 {{%[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:    #dbg_value(ptr undef, ![[K_COPYABLE_LET_CCFLOW_METADATA]], !DIExpression(), ![[ADDR_LOC]]
public func copyableValueArgCCFlowTest(_ k: __owned Klass) {
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo037copyableVarTestCCFlowReinitOutOfBlockF0yyF"(
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK:      br label %[[CONT_BB:[0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// TODO: Should this be a deref like the original?
// CHECK: #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func copyableVarTestCCFlowReinitOutOfBlockTest() {
    var k = Klass()
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
    k = Klass()
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo040copyableVarArgTestCCFlowReinitOutOfBlockG0yyAA5KlassCzF"(
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK:      br label %[[CONT_BB:[0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// CHECK: #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func copyableVarArgTestCCFlowReinitOutOfBlockTest(_ k: inout Klass) {
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
    k = Klass()
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo034copyableVarTestCCFlowReinitInBlockF0yyF"(
// CHECK: entry:
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK:      #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
//
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// CHECK: ret void
// CHECK-NEXT: }
public func copyableVarTestCCFlowReinitInBlockTest() {
    var k = Klass()
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
        k = Klass()
    }
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo037copyableVarArgTestCCFlowReinitInBlockG0yyAA5KlassCzF"(
// CHECK: entry:
// CHECK: #dbg_declare(ptr %m.debug,
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK:      #dbg_value(ptr [[VAR]], ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
//
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// CHECK: ret void
// CHECK-NEXT: }
public func copyableVarArgTestCCFlowReinitInBlockTest(_ k: inout Klass) {
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
        k = Klass()
    }
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo040addressOnlyVarTestCCFlowReinitOutOfBlockG0yyxmAA1PRzlF"(
// CHECK: entry:
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// TODO: Should this be a deref like the original?
// CHECK: #dbg_value(ptr [[VAR]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func addressOnlyVarTestCCFlowReinitOutOfBlockTest<T : P>(_ x: T.Type) {
    var k = T.value
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
    k = T.value
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo043addressOnlyVarArgTestCCFlowReinitOutOfBlockH0yyAA1P_pz_xmtAaCRzlF"(
// CHECK: entry:
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// TODO: Should this be a deref like the original?
// CHECK: #dbg_value(ptr [[VAR]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
// CHECK: ret void
// CHECK-NEXT: }
public func addressOnlyVarArgTestCCFlowReinitOutOfBlockTest<T : P>(_ k: inout (any P), _ x: T.Type) {
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
    }
    k = T.value
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo037addressOnlyVarTestCCFlowReinitInBlockG0yyxmAA1PRzlF"(
// CHECK: entry:
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK:      #dbg_value(ptr [[VAR]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
//
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// CHECK: ret void
// CHECK-NEXT: }
public func addressOnlyVarTestCCFlowReinitInBlockTest<T : P>(_ x: T.Type) {
    var k = T.value
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
        k = T.value
    }
    k.doSomething()
}

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo040addressOnlyVarArgTestCCFlowReinitInBlockH0yyAA1P_pz_xmtAaCRzlF"(
// CHECK: entry:
// CHECK: #dbg_value(ptr [[VAR:%.*]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA:[0-9]+]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC:[0-9]*]]
//
// CHECK:      br i1 %{{[0-9]+}}, label %[[LHS:[0-9]+]], label %[[RHS:[0-9]+]],
//
// CHECK: [[LHS]]:
// CHECK:      #dbg_value(ptr undef, ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref), ![[ADDR_LOC]]
// TODO: Should this be a deref like the original?
// CHECK:      #dbg_value(ptr [[VAR]], ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]], !DIExpression(DW_OP_deref, DW_OP_deref), ![[ADDR_LOC]]
//
// CHECK:      br label %[[CONT_BB:[a-z\.0-9]+]],
//
// CHECK: [[RHS]]:
// CHECK:      br label %[[CONT_BB]],
//
// CHECK: [[CONT_BB]]:
// CHECK: ret void
// CHECK-NEXT: }
public func addressOnlyVarArgTestCCFlowReinitInBlockTest<T : P>(_ k: inout (any P), _ x: T.Type) {
    k.doSomething()
    if trueValue {
        let m = consume k
        m.doSomething()
        k = T.value
    }
    k.doSomething()
}

//////////////////////////
// Late Checks //
//////////////////////////

// CHECK-DAG: ![[K_COPYABLE_VALUE_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_COPYABLE_VAR_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_ADDR_LET_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_ADDRONLY_VAR_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_COPYABLE_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_COPYABLE_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_OUT_BLOCK_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_ADDRESSONLY_VAR_CCFLOW_REINIT_IN_BLOCK_METADATA]] = !DILocalVariable(name: "k",
// CHECK-DAG: ![[K_COPYABLE_LET_CCFLOW_METADATA]] = !DILocalVariable(name: "k",
