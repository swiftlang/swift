// RUN: %target-swift-frontend -g -emit-ir -enable-anonymous-context-mangled-names %s | %FileCheck %s

public protocol P {}
extension Int: P {} 

// CHECK: [[DEFINER_NAME:@.*]] = {{.*}}constant [{{[0-9]+}} x i8] c"$s24opaque_result_type_debug3fooQryF\00"
// CHECK: @"$s24opaque_result_type_debug3fooQryFMXX" = {{.*}}constant{{.*}} [[DEFINER_NAME]]
// CHECK: @"$s24opaque_result_type_debug3fooQryFQOMQ" = {{.*}}constant{{.*}} @"$s24opaque_result_type_debug3fooQryFMXX"

// CHECK: @"\01l_type_metadata_table" = {{.*}} @"$s24opaque_result_type_debug3fooQryFQOMQ"

public func foo() -> some P {
  return 0
}

@_silgen_name("use") public func use<T: P>(_: T)

public func bar<T: P>(genericValue: T) {
  use(genericValue)

  let intValue = 0
  use(intValue)

  let opaqueValue = foo()
  use(opaqueValue)
}

// CHECK: [[OPAQUE_TYPE:![0-9]+]] = !DICompositeType({{.*}} name: "$s24opaque_result_type_debug3fooQryFQOyQo_D"
// CHECK: {{![0-9]+}} = !DILocalVariable(name: "opaqueValue",{{.*}} type: [[OPAQUE_TYPE]])
