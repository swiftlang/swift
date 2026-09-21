// RUN: %target-swift-frontend -emit-ir %s -enforce-exclusivity=checked -enable-experimental-feature Embedded -disable-experimental-feature EmbeddedDynamicExclusivity -module-name exclusivity_builtins -parse-stdlib -parse-as-library  -enforce-exclusivity=unchecked | %FileCheck %s

// REQUIRES: swift_feature_Embedded

@_silgen_name("marker1")
func marker1() -> ()

@_silgen_name("marker2")
func marker2() -> ()

@_silgen_name("marker3")
func marker3() -> ()

public struct MyClass {
  var i: Builtin.Int32
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$e20exclusivity_builtins11beginAccessyyBp_BpAA7MyClassVmtF"(ptr %0, ptr %1)
// CHECK-NOT:   call void @swift_beginAccess
// CHECK: ret void

@used
public func beginAccess(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: MyClass.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$e20exclusivity_builtins9endAccessyyBpF"(ptr{{.*}})
// CHECK-NOT:   call void @swift_endAccess
// CHECK: ret void
@used
public func endAccess(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$e20exclusivity_builtins10readAccessyyBp_AA7MyClassVmtF"(ptr %0)
// CHECK-NOT:   call void @swift_beginAccess
// CHECK: ret void
@used
public func readAccess(_ address: Builtin.RawPointer, _ ty1: MyClass.Type) {
  marker3()
  Builtin.performInstantaneousReadAccess(address, ty1);
}
