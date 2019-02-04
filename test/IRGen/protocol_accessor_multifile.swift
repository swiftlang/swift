// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/protocol_accessor_multifile_other.swift > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.ll

// CHECK: @"$s27protocol_accessor_multifile5ProtoMp" = external{{( dllimport)?}} global
// NEGATIVE-NOT: @"$s27protocol_accessor_multifile10ClassProtoMp" =

// CHECK-LABEL: define{{.*}} void @"$s27protocol_accessor_multifile14useExistentialyyF"()
func useExistential() {
  // CHECK: [[BOX:%.+]] = alloca %T27protocol_accessor_multifile5ProtoP,
  // CHECK: call swiftcc void @"$s27protocol_accessor_multifile17globalExistentialAA5Proto_pvg"({{%.+}} [[BOX]])
  // CHECK: call swiftcc void @"$s27protocol_accessor_multifile5ProtoPAAE6methodyyF"
  globalExistential.method()
  // CHECK: call void @__swift_destroy_boxed_opaque_existential_1({{%.+}} [[BOX]])
  // CHECK: ret void
}

class GenericContext<T: Proto> {
  // CHECK-LABEL: define{{.*}} void @"$s27protocol_accessor_multifile14GenericContextC04testdE0yyFZ
  static func testGenericContext() {
    // CHECK: [[SELF:%.+]] = bitcast %swift.type* %0 to %swift.type**
    // CHECK: [[WITNESS_TABLE:%.+]] = getelementptr inbounds %swift.type*, %swift.type** [[SELF]],
    // CHECK: = load %swift.type*, %swift.type** [[WITNESS_TABLE]]
    // CHECK: ret void
  }
}

// CHECK-LABEL: define{{.*}} void @"$s27protocol_accessor_multifile19useClassExistentialyyF"()
func useClassExistential() {
  let g = getClassExistential()
  // CHECK: [[G_TYPE:%.+]] = call %swift.type* @swift_getObjectType({{%.+}} {{%.+}})
  // CHECK: call swiftcc void {{%.+}}(i{{32|64}} 1, {{%.+}} {{%.+}}, %swift.type* [[G_TYPE]], i8** {{%.+}})
  g?.baseProp = 1
  // CHECK: ret void
}
