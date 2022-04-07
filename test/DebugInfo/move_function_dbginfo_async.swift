// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -disable-availability-checking -g -emit-ir -o - %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -disable-availability-checking -g -c %s -o %t/out.o
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
// REQUIRES: CPU=x86_64 || CPU=arm64
// REQUIRES: optimized_stdlib

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

public func use<T>(_ t: T) {}
public func forceSplit() async {}

///////////
// Tests //
///////////

// CHECK-LABEL: define swifttailcc void @"$s27move_function_dbginfo_async13letSimpleTestyyxnYalF"(%swift.context* swiftasync %0, %swift.opaque* noalias %1, %swift.type* %T)
// CHECK: entry:
// CHECK:   call void @llvm.dbg.addr(metadata %swift.context* %0, metadata ![[SIMPLE_TEST_METADATA:[0-9]+]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK:   musttail call swifttailcc void
// CHECK-NEXT: ret void

// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13letSimpleTestyyxnYalFTQ0_"(i8* swiftasync %0)
// CHECK: entryresume.0:
// CHECK:   call void @llvm.dbg.addr(metadata i8* %0, metadata ![[SIMPLE_TEST_METADATA_2:[0-9]+]], metadata !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)),
// CHECK:   musttail call swifttailcc void
// CHECK-NEXT: ret void
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13letSimpleTestyyxnYalFTY1_"(i8* swiftasync %0)
// CHECK: entryresume.1:
// CHECK:     call void @llvm.dbg.addr(metadata i8* %0, metadata ![[SIMPLE_TEST_METADATA_3:[0-9]+]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK:     call void @llvm.dbg.value(metadata %swift.opaque* undef, metadata ![[SIMPLE_TEST_METADATA_3]], metadata !DIExpression(DW_OP_deref)), !dbg ![[ADDR_LOC]]
// CHECK:   musttail call swifttailcc void
// CHECK-NEXT: ret void

// DWARF:  DW_AT_linkage_name	("$s3out13letSimpleTestyyxnYalF")
// DWARF:  DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG:DW_OP_.*]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT:  DW_AT_name ("msg")
//
// DWARF:  DW_AT_linkage_name	("$s3out13letSimpleTestyyxnYalFTQ0_")
// DWARF:  DW_AT_name	("letSimpleTest")
// DWARF:  DW_TAG_formal_parameter
// DWARF-NEXT:  DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC:[a-f0-9]+]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT:  DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13letSimpleTestyyxnYalFTY1_")
// DWARF: DW_AT_name	("letSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:            [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT:            DW_AT_name	("msg")
public func letSimpleTest<T>(_ msg: __owned T) async {
    await forceSplit()
    use(_move(msg))
}

// CHECK-LABEL: define swifttailcc void @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalF"(%swift.context* swiftasync %0, %swift.opaque* %1, %swift.opaque* noalias %2, %swift.type* %T)
// CHECK:   call void @llvm.dbg.addr(metadata %swift.context* %0, metadata !{{[0-9]+}}, metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref))
// CHECK:   musttail call swifttailcc void @"$s27move_function_dbginfo_async10forceSplityyYaF"(%swift.context* swiftasync %{{[0-9]+}})
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalFTQ0_"(i8* swiftasync %0)
// CHECK: entryresume.0:
// CHECK:   call void @llvm.dbg.addr(metadata i8* %0, metadata !{{[0-9]+}}, metadata !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref))
// CHECK: musttail call swifttailcc void @swift_task_switch(%swift.context* swiftasync %9, i8* bitcast (void (i8*)* @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalFTY1_" to i8*), i64 0, i64 0)
// CHECK-NEXT: ret void
// CHECK-NEXT: }
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalFTY1_"(i8* swiftasync %0)
// CHECK: entryresume.1:
// CHECK:   call void @llvm.dbg.addr(metadata i8* %0, metadata ![[METADATA:[0-9]+]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK:   call void @llvm.dbg.value(metadata %swift.opaque* undef, metadata ![[METADATA]], metadata !DIExpression(DW_OP_deref)), !dbg ![[ADDR_LOC]]
// CHECK:   musttail call swifttailcc void @"$s27move_function_dbginfo_async10forceSplityyYaF"(%swift.context* swiftasync
// CHECK-NEXT: ret void
// CHECK-NEXT: }
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalFTQ2_"(i8* swiftasync %0)
// CHECK: entryresume.2:

// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async13varSimpleTestyyxz_xtYalFTY3_"(i8* swiftasync %0)
// CHECK: entryresume.3:
// CHECK:    call void @llvm.dbg.addr(metadata i8* %0, metadata ![[METADATA:[0-9]+]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK:   call void @llvm.dbg.value(metadata %swift.opaque* undef, metadata ![[METADATA]], metadata !DIExpression(DW_OP_deref)), !dbg ![[ADDR_LOC]]
// CHECK:   call void @llvm.dbg.addr(metadata i8* %0, metadata ![[METADATA]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8, DW_OP_deref)), !dbg ![[ADDR_LOC]]
// CHECK: musttail call swifttailcc void @"$s27move_function_dbginfo_async10forceSplityyYaF"(
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalF")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name ("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ0_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC:[a-f0-9]+]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY1_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// We were just moved and are not reinit yet. This is caused by us hopping twice
// when we return from an async function. Once for the async function and then
// for the hop to executor.
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ2_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_name ("msg")
//
// We reinitialize our value in this funclet and then move it and then
// reinitialize it again. So we have two different live ranges. Sadly, we don't
// validate that the first live range doesn't start at the beginning of the
// function. But we have lldb tests to validate that.
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY3_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}):
// DWARF-SAME:        DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}):
// DWARF-SAME:        DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref
// DWARF-NEXT: DW_AT_name	("msg")
//
// We did not move the value again here, so we just get a normal entry value for
// the entire function.
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ4_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY5_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref
// DWARF-NEXT: DW_AT_name	("msg")

// Change name to varSimpleTestArg
public func varSimpleTest<T>(_ msg: inout T, _ msg2: T) async {
    await forceSplit()
    use(_move(msg))
    await forceSplit()
    msg = msg2
    let msg3 = _move(msg)
    let _ = msg3
    msg = msg2
    await forceSplit()
}

// We don't have an argument here, so we shouldn't have an llvm.dbg.addr in the
// initial function.
//
// CHECK-LABEL: define swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaF"(%swift.context* swiftasync %0)
// CHECK-NOT: llvm.dbg.addr
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaFTY0_"(i8* swiftasync %0)
// CHECK: call void @llvm.dbg.addr(metadata i8* %0, metadata !{{[0-9]+}}, metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8))
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaFTQ1_"(i8* swiftasync %0)
// CHECK: call void @llvm.dbg.addr(metadata i8* %0, metadata !{{[0-9]+}}, metadata !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8))

// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaFTY2_"(i8* swiftasync %0)
// CHECK: call void @llvm.dbg.addr(metadata i8* %0, metadata ![[METADATA:[0-9]+]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8)), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK: call void @llvm.dbg.value(metadata %T27move_function_dbginfo_async5KlassC** undef, metadata ![[METADATA]], metadata !DIExpression()), !dbg ![[ADDR_LOC]]

// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaFTQ3_"(i8* swiftasync %0)
// We should only see an llvm.dbg.value here.
// CHECK-NOT: llvm.dbg.addr
// CHECK: call void @llvm.dbg.value(metadata %T27move_function_dbginfo_async5KlassC** undef,
// CHECK-NOT: llvm.dbg.addr
//
// We should see first a llvm.dbg.value to undef the value until we reinit. Then
// we should see a llvm.dbg.addr to reinit.
//
// CHECK-LABEL: define internal swifttailcc void @"$s27move_function_dbginfo_async16varSimpleTestVaryyYaFTY4_"(i8* swiftasync %0)
// CHECK: call void @llvm.dbg.value(metadata %T27move_function_dbginfo_async5KlassC** undef, metadata ![[METADATA:[0-9]+]], metadata !DIExpression()), !dbg ![[ADDR_LOC:[0-9]+]]
// CHECK: call void @llvm.dbg.addr(metadata i8* %0, metadata ![[METADATA]], metadata !DIExpression(DW_OP_plus_uconst, 16, DW_OP_plus_uconst, 8)), !dbg ![[ADDR_LOC]]

// We are not an argument, so no problem here.
//
// DWARF: DW_AT_linkage_name	("$s3out16varSimpleTestVaryyYaF")
//
// DWARF: DW_AT_linkage_name	("$s3out16varSimpleTestVaryyYaFTY0_")
//
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location   (DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8)
// DWARF-NEXT: DW_AT_name       ("k")
//
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location
// DWARF-NEXT: DW_AT_name ("m")
//
// DWARF: DW_AT_linkage_name	("$s3out16varSimpleTestVaryyYaFTQ1_")
//
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location   (DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8)
// DWARF-NEXT: DW_AT_name       ("k")
//
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x10)
// DWARF-NEXT: DW_AT_name ("m")
//
// DWARF: DW_AT_linkage_name	("$s3out16varSimpleTestVaryyYaFTY2_")
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location   (0x{{[0-9a-f]+}}:
// DWARF-NEXT:    [0x{{[0-9a-f]+}}, 0x{{[0-9a-f]+}}): DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8)
// DWARF-NEXT: DW_AT_name       ("k")
// DWARF:    DW_TAG_variable
// DWARF-NEXT: DW_AT_location
// DWARF-NEXT: DW_AT_name ("m")
//
// DWARF: DW_AT_linkage_name  ("$s3out16varSimpleTestVaryyYaFTQ3_")
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (DW_OP_entry_value([[ASYNC_REG]]), DW_OP_deref, DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x10)
// DWARF-NEXT: DW_AT_name  ("m")
// K is dead here.
// DWARF: DW_TAG_variable
// DWARF-NEXT:    DW_AT_name  ("k")
//
// We reinitialize k in 4.
// DWARF: DW_AT_linkage_name  ("$s3out16varSimpleTestVaryyYaFTY4_")
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x10)
// DWARF-NEXT: DW_AT_name  ("m")
// DWARF: DW_TAG_variable
// DWARF-NEXT: DW_AT_location  (0x{{[0-9a-f]+}}:
// DWARF-NEXT: [0x{{[0-9a-f]+}}, 0x{{[0-9a-f]+}}): DW_OP_entry_value([[ASYNC_REG]]), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8)
// DWARF-NEXT: DW_AT_name ("k")
public func varSimpleTestVar() async {
    var k = Klass()
    k.doSomething()
    await forceSplit()
    let m = _move(k)
    m.doSomething()
    await forceSplit()
    k = Klass()
    k.doSomething()
    print("stop here")
}
