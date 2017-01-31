// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -c -g -o %t.o
// RUN: dwarfdump --apple-types %t.o | %FileCheck --check-prefix=CHECK-ACCEL %s
// RUN: dwarfdump --debug-info %t.o | %FileCheck --check-prefix=CHECK-DWARF %s
// DISABLED <rdar://problem/28232630>: dwarfdump --verify %t.o
// REQUIRES: OS=macosx

// Verify that the unmangles basenames end up in the accelerator table.
// CHECK-ACCEL-DAG:	 str[0]{{.*}}"Int64"
// CHECK-ACCEL-DAG:	 str[0]{{.*}}"foo"

// Verify that the mangled names end up in the debug info.
// CHECK-DWARF: TAG_module
// CHECK-DWARF: AT_name( "main" )
// CHECK-DWARF: TAG_structure_type
// CHECK-DWARF-NEXT: AT_name( "foo" )
// CHECK-DWARF-NEXT: AT_linkage_name( "_T04main3fooCD" )

// Verify the IR interface:
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "foo"
// CHECK-SAME:             line: [[@LINE+2]]
// CHECK-SAME:             identifier: "_T04main3fooCD"
class foo {
	var x : Int64 = 1
}

func main() -> Int64 {
	var thefoo = foo();
	return thefoo.x
}

main()
