// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -target %target-swift-5.1-abi-triple -emit-module -emit-module-path %t/opaque_result_type_debug_other.swiftmodule -module-name opaque_result_type_debug_other -enable-anonymous-context-mangled-names %s -DLIBRARY
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -g -emit-ir -enable-anonymous-context-mangled-names %s -DCLIENT -I %t | %FileCheck %s

#if LIBRARY

public protocol P {}
extension Int: P {} 

public func foo() -> some P {
  return 0
}

public var prop: some P {
  return 0
}

public struct Foo {
  public init() {}
  public subscript() -> some P {
    return 0
  }
}

public class C: P { }

public func bar() -> [(some P, (some AnyObject & P)?)] {
  return [(1, C())]
}

#else

import opaque_result_type_debug_other

@_silgen_name("use") public func use<T: P>(_: T)

@inlinable
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

  let opaqueArray = bar()
  let opaqueArrayFirst = opaqueArray[0].0
  use(opaqueArrayFirst)
  let opaqueArraySecond = opaqueArray[0].1!
  use(opaqueArraySecond)
}

#endif

// CHECK-DAG: ![[OPAQUE_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s30opaque_result_type_debug_other3fooQryFQOyQo_D"
// CHECK-DAG: ![[LET_OPAQUE_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_TYPE]])
// CHECK-DAG: ![[OPAQUE_PROP_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s30opaque_result_type_debug_other4propQrvpQOyQo_D"
// CHECK-DAG: ![[LET_OPAQUE_PROP_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_PROP_TYPE]])
// CHECK-DAG: ![[OPAQUE_SUB_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s30opaque_result_type_debug_other3FooVQrycipQOy_Qo_D"
// CHECK-DAG: ![[LET_OPAQUE_SUB_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_SUB_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueValue",{{.*}} type: ![[LET_OPAQUE_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaquePropValue",{{.*}} type: ![[LET_OPAQUE_PROP_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueSubValue",{{.*}} type: ![[LET_OPAQUE_SUB_TYPE]])
// CHECK-DAG: ![[OPAQUE_ARRAY_FIRST_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s30opaque_result_type_debug_other3barSayQr_QR_SgtGyFQOyQo_D"
// CHECK-DAG: ![[LET_OPAQUE_ARRAY_FIRST_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_ARRAY_FIRST_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueArrayFirst",{{.*}} type: ![[LET_OPAQUE_ARRAY_FIRST_TYPE]])
// CHECK-DAG: ![[OPAQUE_ARRAY_SECOND_TYPE:[0-9]+]] = !DICompositeType({{.*}} name: "$s30opaque_result_type_debug_other3barSayQr_QR_SgtGyFQOyQo0_D"
// CHECK-DAG: ![[LET_OPAQUE_ARRAY_SECOND_TYPE:[0-9]+]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[OPAQUE_ARRAY_SECOND_TYPE]])
// CHECK-DAG: {{![0-9]+}} = !DILocalVariable(name: "opaqueArraySecond",{{.*}} type: ![[LET_OPAQUE_ARRAY_SECOND_TYPE]])
