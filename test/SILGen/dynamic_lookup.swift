// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

class X {
  func [objc] f() { }
}

protocol [class_protocol,objc] P {
  func g()
}

// CHECK-LABEL: sil  @_T14dynamic_lookup15direct_to_classFT3objPSo13DynamicLookup__T_
func direct_to_class(obj : DynamicLookup) {
  // CHECK: [[OBJ:%[0-9]+]] = project_existential_ref [[EX:%[0-9]+]] : $DynamicLookup
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ]] : $Builtin.ObjCPointer, #X.f!1.objc : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> ()
  // CHECK: apply [[METHOD]]([[OBJ]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> ()
  obj.f!()
}

// CHECK-LABEL: sil  @_T14dynamic_lookup18direct_to_protocolFT3objPSo13DynamicLookup__T_
func direct_to_protocol(obj : DynamicLookup) {
  // CHECK: [[OBJ:%[0-9]+]] = project_existential_ref [[EX:%[0-9]+]] : $DynamicLookup
  // CHECK: [[METHOD:%[0-9]+]] = dynamic_method [volatile] [[OBJ]] : $Builtin.ObjCPointer, #P.g!1.objc : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> ()
  // CHECK: apply [[METHOD]]([[OBJ]]) : $[cc(objc_method), thin] (Builtin.ObjCPointer, ()) -> ()
  obj.g!()
}
