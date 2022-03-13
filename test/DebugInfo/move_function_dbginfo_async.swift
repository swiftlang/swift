// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -disable-availability-checking -Xllvm -sil-disable-pass=alloc-stack-hoisting -g -emit-ir -o - %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -disable-availability-checking -Xllvm -sil-disable-pass=alloc-stack-hoisting -g -c %s -o %t/out.o
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
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT:  DW_AT_name ("msg")
//
// DWARF:  DW_AT_linkage_name	("$s3out13letSimpleTestyyxnYalFTQ0_")
// DWARF:  DW_AT_name	("letSimpleTest")
// DWARF:  DW_TAG_formal_parameter
// DWARF-NEXT:  DW_AT_location	(DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC:[a-f0-9]+]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT:  DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13letSimpleTestyyxnYalFTY1_")
// DWARF: DW_AT_name	("letSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:            [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
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
// CHECK:   musttail call swifttailcc void @"$s27move_function_dbginfo_async10forceSplityyYaF"(%swift.context* swiftasync %34)
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
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name ("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ0_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC:[a-f0-9]+]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY1_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// TODO: Missing debug info in s3out13varSimpleTestyyxz_xtYalFTQ2_
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ2_")
// DWARF: DW_AT_name	("varSimpleTest")
//
// We perform moves in this funclet so we at first have an entry_value value
// that is moved and then we use a normal register.
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY3_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}):
// DWARF-SAME:        DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref
// DWARF-NEXT:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}})
// DWARF-SAME:        DW_OP_breg{{.*}}, DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTQ4_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF-NEXT: DW_AT_location	(DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_deref, DW_OP_plus_uconst 0x[[MSG_LOC]], DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF-NEXT: DW_AT_name	("msg")
//
// DWARF: DW_AT_linkage_name	("$s3out13varSimpleTestyyxz_xtYalFTY5_")
// DWARF: DW_AT_name	("varSimpleTest")
// DWARF: DW_TAG_formal_parameter
// DWARF: DW_AT_location	(0x{{[a-f0-9]+}}:
// DWARF:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_entry_value(DW_OP_reg14 R14), DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref
// DWARF:    [0x{{[a-f0-9]+}}, 0x{{[a-f0-9]+}}): DW_OP_breg6 RBP-88, DW_OP_deref, DW_OP_plus_uconst 0x10, DW_OP_plus_uconst 0x8, DW_OP_deref)
// DWARF: DW_AT_name	("msg")

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
