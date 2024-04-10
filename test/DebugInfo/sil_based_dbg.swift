// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false %s -O -sil-based-debuginfo -Xllvm -sil-print-debuginfo -emit-ir -o %t/out.ir
// RUN: %FileCheck %s < %t/out.ir
// RUN: %FileCheck %s --check-prefix=CHECK_OUT_SIL < %t/out.ir.sil_dbg_0.sil

// Second test: check that we don't crash with multi-threaded IRGen
// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -c %s %S/Inputs/testclass.swift -wmo -O -num-threads 1 -sil-based-debuginfo -o %t/sil_based_dbg.o -o %t/testclass.o

// CHECK: !DIFile(filename: "{{.+}}sil_based_dbg.swift", directory: "{{.+}}")
// CHECK: [[F:![0-9]+]] = !DIFile(filename: "{{.+}}out.ir.sil_dbg_0.sil",
// CHECK: !DISubprogram(linkageName: "$s3out6testityyF", scope: !{{[0-9]+}}, file: [[F]], line: {{[1-9][0-9]+}},

// CHECK_OUT_SIL: sil @$s3out6testityyF : $@convention(thin) () -> () {
public func testit() {
  print("Hello")
}

// We need to remove the debug scope within alloc_stack's auxiliary debug var info
// in sil-based-dbg mode.
// To create something like `alloc_stack ..., (name "foo", loc ..., scope 0)...`
// as our testing input, we're only running SROA over the input swift code.
// RUN: %target-swift-frontend -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false %s -disable-debugger-shadow-copies -emit-sil -g -o %t/stage1.sil
// RUN: %target-sil-opt -parse-serialized-sil -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false -sil-print-debuginfo -access-marker-elim -sroa %t/stage1.sil -o %t/stage2.sil
// The verification shouldn't fail
// RUN: %target-swift-frontend -Xllvm -parse-serialized-sil -enable-copy-propagation=requested-passes-only -enable-lexical-lifetimes=false %t/stage2.sil -sil-verify-all -sil-based-debuginfo -g -emit-sil -o %t/out.sil
// RUN: %FileCheck %s --check-prefix=CHECK_DBG_SCOPE < %t/out.sil
struct TheStruct {
    var the_member : Int
}
// CHECK_DBG_SCOPE-LABEL: sil {{.*}}test_debug_scope
public func test_debug_scope(val : Int) -> Int {
    // CHECK_DBG_SCOPE: alloc_stack $Builtin.Int{{[0-9]+}},
    // CHECK_DBG_SCOPE-SAME:  var, (name "the_struct",
    // CHECK_DBG_SCOPE-SAME:  loc
    // CHECK_DBG_SCOPE-SAME:  expr op_fragment:#TheStruct.the_member:op_fragment:#Int._value
    // The auxiliary debug scope should be removed
    // CHECK_DBG_SCOPE-NOT:   scope {{[0-9]+}})
    var the_struct = TheStruct(the_member: 0)
    the_struct.the_member = val + 13
    return the_struct.the_member
}
