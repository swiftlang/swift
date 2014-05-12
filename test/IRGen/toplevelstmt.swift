// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir | FileCheck %s

// CHECK: define internal void @top_level_code()
// CHECK:  alloca %Si, align 8
// CHECK:  alloca %Si, align 8

// CHECK: define i32 @main(i32 %argc, i8** %argv) {
// CHECK-NEXT: entry:
// CHECK-NEXT: %0 = call i8* @_TFSsa6C_ARGCVSs5Int32()
// CHECK-NEXT: %1 = bitcast i8* %0 to i32*
// CHECK-NEXT: store i32 %argc, i32* %1
// CHECK-NEXT: %2 = call i8* @_TFSsa6C_ARGVGVSs13UnsafePointerVSs7CString_()
// CHECK-NEXT: %3 = bitcast i8* %2 to i8***
// CHECK-NEXT: store i8** %argv, i8*** %3
// CHECK-NEXT: call void @top_level_code()
// CHECK-NEXT: ret i32 0

// CCP barrier for the branch below
@asmname("condition") func condition() -> Bool
if (condition()) {
  var x = 3
  print(x)
} else {
  var x = 3
  print(x)
}
