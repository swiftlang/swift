// RUN: %swift -emit-ir -target x86_64-apple-macosx10.9 -o - %s | FileCheck %s

// Currently, this can't be a SIL file because we don't parse
// conformances correctly.

protocol Kindable {
  var kind: Int { get }
}

extension Int: Kindable {
  var kind: Int { return 0 }
}

extension Float: Kindable {
  var kind: Int { return 1 }
}

// CHECK: define hidden void @_TF21existential_metatypes5test0FT_T_()
// CHECK:      [[V:%.*]] = alloca { %swift.type*, i8** }, align 8
func test0() {
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { %swift.type*, i8** }* [[V]], i32 0, i32 0
// CHECK-NEXT: store %swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1), %swift.type** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { %swift.type*, i8** }* [[V]], i32 0, i32 1
// CHECK-NEXT: store i8** getelementptr inbounds ([1 x i8*]* @_TWPSi21existential_metatypes8Kindable, i32 0, i32 0), i8*** [[T0]], align 8
  var v: Kindable.Type = Int.self

// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { %swift.type*, i8** }* [[V]], i32 0, i32 0
// CHECK-NEXT: store %swift.type* getelementptr inbounds (%swift.full_type* @_TMdSf, i32 0, i32 1), %swift.type** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { %swift.type*, i8** }* [[V]], i32 0, i32 1
// CHECK-NEXT: store i8** getelementptr inbounds ([1 x i8*]* @_TWPSf21existential_metatypes8Kindable, i32 0, i32 0), i8*** [[T0]], align 8
  v = Float.self

// CHECK-NEXT: ret void
}
