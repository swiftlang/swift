// RUN: %target-swift-frontend -disable-availability-checking -g -emit-ir -enable-anonymous-context-mangled-names %s | %FileCheck %s

public protocol P {}
extension Int: P {} 

// CHECK: [[DEFINER_NAME:@.*]] = {{.*}}constant [{{[0-9]+}} x i8] c"$s24opaque_result_type_debug3fooQryF\00"
// CHECK: @"$s24opaque_result_type_debug3fooQryFMXX" = {{.*}}constant{{.*}} [[DEFINER_NAME]]
// CHECK: @"$s24opaque_result_type_debug3fooQryFQOMQ" = {{.*}}constant{{.*}} @"$s24opaque_result_type_debug3fooQryFMXX"

public func foo() -> some P {
  return 0
}

// CHECK: [[DEFINER_NAME:@.*]] = {{.*}}constant [{{[0-9]+}} x i8] c"$s24opaque_result_type_debug4propQrvp\00"
// CHECK: @"$s24opaque_result_type_debug4propQrvpMXX" = {{.*}}constant{{.*}} [[DEFINER_NAME]]
// CHECK: @"$s24opaque_result_type_debug4propQrvpQOMQ" = {{.*}}constant{{.*}} @"$s24opaque_result_type_debug4propQrvpMXX"

public var prop: some P {
  return 0
}

// CHECK: [[DEFINER_NAME:@.*]] = {{.*}}constant [{{[0-9]+}} x i8] c"$s24opaque_result_type_debug3FooVQrycip\00"
// CHECK: @"$s24opaque_result_type_debug3FooVQrycipMXX" = {{.*}}constant{{.*}} [[DEFINER_NAME]]
// CHECK: @"$s24opaque_result_type_debug3FooVQrycipQOMQ" = {{.*}}constant{{.*}} @"$s24opaque_result_type_debug3FooVQrycipMXX"

public struct Foo {
  public subscript() -> some P {
    return 0
  }
}

// CHECK: @"\01l_type_metadata_table" = {{.*}} @"$s24opaque_result_type_debug3fooQryFQOMQ"

@_silgen_name("use") public func use<T: P>(_: T)

public func bar<T: P>(genericValue: T) {
  use(genericValue)

  let intValue = 0
  use(intValue)

  let opaqueValue = foo()
  use(opaqueValue)

  let opaquePropValue = prop
  use(opaquePropValue)

  let opaqueSubValue = Foo()[]
  use(opaqueSubValue)
}

// CHECK-DAG: ![[OPAQUE_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s24opaque_result_type_debug3fooQryFQOyQo_D"
// CHECK-DAG: ![[LET_OPAQUE_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_TYPE]])
// CHECK-DAG: ![[OPAQUE_PROP_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s24opaque_result_type_debug4propQrvpQOyQo_D"
// CHECK-DAG: ![[LET_OPAQUE_PROP_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_PROP_TYPE]])
// CHECK-DAG: ![[OPAQUE_SUB_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s24opaque_result_type_debug3FooVQrycipQOy_Qo_D"
// CHECK-DAG: ![[LET_OPAQUE_SUB_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_SUB_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueValue",{{.*}} type: ![[LET_OPAQUE_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaquePropValue",{{.*}} type: ![[LET_OPAQUE_PROP_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueSubValue",{{.*}} type: ![[LET_OPAQUE_SUB_TYPE]])
