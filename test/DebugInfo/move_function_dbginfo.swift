// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -g -emit-ir -o - %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -g -c %s -o %t/out.o
// RUN: %llvm-dwarfdump --show-children %t/out.o | %FileCheck -check-prefix=DWARF %s

// This test checks that:
//
// 1. At the IR level, we insert the appropriate llvm.dbg.addr, llvm.dbg.value.
//
// 2. At the Dwarf that we have proper locations with PC validity ranges where
//    the value isn't valid.

// We only run this on macOS right now since we would need to pattern match
// slightly differently on other platforms.
// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

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

///////////
// Tests //
///////////

// CHECK-LABEL: define swiftcc void @"$s21move_function_dbginfo17copyableValueTestyyF"()
//
// We should have a llvm.dbg.addr for k since we moved it.
// CHECK: call void @llvm.dbg.addr(metadata {{.*}}** %k.debug, metadata ![[K_COPYABLE_VALUE_TEST:[0-9]*]],
//
// In contrast, we should have a dbg.declare for m since we aren't
// CHECK: call void @llvm.dbg.declare(metadata {{.*}}** %m.debug, metadata ![[M_COPYABLE_VALUE_TEST:[0-9]*]],
//
// Our undef should be an llvm.dbg.value. Counter-intuitively this works for
// both llvm.dbg.addr /and/ llvm.dbg.value. Importantly though its metadata
// should be for k since that is the variable that we are telling the debugger
// is no longer defined.
// CHECK: call void @llvm.dbg.value(metadata %T21move_function_dbginfo5KlassC* undef, metadata ![[K_COPYABLE_VALUE_TEST]],
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
// DWARF-NEXT: DW_AT_location	(DW_OP_fbreg
// DWARF-NEXT: DW_AT_name	("m")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
//
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(0x{{[a-z0-9]+}}:
// DWARF-NEXT:    [0x{{[a-z0-9]+}}, 0x{{[a-z0-9]+}}):
// DWARF-NEXT: DW_AT_name	("k")
// DWARF-NEXT: DW_AT_decl_file	(
// DWARF-NEXT: DW_AT_decl_line	(
// DWARF-NEXT: DW_AT_type	(
public func copyableValueTest() {
    let k = Klass()
    k.doSomething()
    let m = _move(k)
    m.doSomething()
}

// TODO: Look at this.
public func copyableVarTest() {
    var k = Klass()
    k.doSomething()
    let m = _move(k)
    m.doSomething()
    k = Klass()
    k.doSomething()
}

public func addressOnlyValueTest<T : P>(_ x: T) {
    let k = x
    k.doSomething()
    let m = _move(k)
    m.doSomething()
}

public func addressOnlyVarTest<T : P>(_ x: T) {
    var k = x
    k.doSomething()
    let m = _move(k)
    m.doSomething()
    k = x
    k.doSomething()
}
