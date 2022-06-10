// RUN: %target-swift-frontend -O -enable-opt-in-reflection-metadata -emit-ir %s | %FileCheck %s --check-prefix=CHECK-REL
// RUN: %target-swift-frontend -enable-opt-in-reflection-metadata -emit-ir %s | %FileCheck %s --check-prefix=CHECK-DEB

// reflection metadata field descriptor opt_in_reflection_metadata.RefProtocol
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata11RefProtocol_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata11RefProtocol_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public protocol RefProtocol: Reflectable {
  associatedtype RefInner
  var inner: RefInner { get }
}

// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata12RefProtocol2_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public protocol RefProtocol2: RefProtocol {}

// reflection metadata field descriptor opt_in_reflection_metadata.Conformance
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata11ConformanceVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata11ConformanceVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public struct Conformance: RefProtocol {
  public var inner: Int = 0
}

// reflection metadata field descriptor opt_in_reflection_metadata.RefStruct
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata9RefStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata9RefStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public struct RefStruct: Reflectable {
  let i: Int
  let rc: RefClass
  let re: RefEnum
}

// reflection metadata field descriptor opt_in_reflection_metadata.RefEnum
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata7RefEnumOMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public enum RefEnum: Reflectable {
  case C(RefClass)
  indirect case S(RefStruct)
  indirect case E(RefEnum)
  case I(Int)
}

// reflection metadata field descriptor opt_in_reflection_metadata.RefClass
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata8RefClassCMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public class RefClass: Reflectable {
  let i: Int
  
  public init(i: Int) {
    self.i = i
  }
}

// reflection metadata field descriptor opt_in_reflection_metadata.RefClassChild
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata13RefClassChildCMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public class RefClassChild: RefClass {
  let y: Int
  
  public init(y: Int) {
    self.y = y
    super.init(i: y)
  }
}

// reflection metadata field descriptor opt_in_reflection_metadata.RefGenericClass
// CHECK-REL-DAG: @"$s26opt_in_reflection_metadata15RefGenericClassCMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public class RefGenericClass<T>: RefProtocol2 {
  public let inner: T

  public init(inner: T) {
    self.inner = inner
  }
}

// reflection metadata field descriptor opt_in_reflection_metadata.NonRefProtocol
// CHECK-REL-NOT: @"$s26opt_in_reflection_metadata14NonRefProtocol_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata14NonRefProtocol_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public protocol NonRefProtocol {
  associatedtype NonRefInner
  var inner: NonRefInner { get }
}

public protocol RefProtocol3: Reflectable {}

// reflection metadata field descriptor opt_in_reflection_metadata.NonRefStruct
// CHECK-REL-NOT: @"$s26opt_in_reflection_metadata12NonRefStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata12NonRefStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public struct NonRefStruct {
  let i: Int
  let y: RefProtocol3
}

// reflection metadata field descriptor opt_in_reflection_metadata.NonRefGenericStruct
// CHECK-REL-NOT: @"$s26opt_in_reflection_metadata19NonRefGenericStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DEB-DAG: @"$s26opt_in_reflection_metadata19NonRefGenericStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
public struct NonRefGenericStruct<T: RefProtocol> {
  let inner: T
  let i: RefProtocol3
}

// CHECK-REL-DAG: @__swift_reflection_version = linkonce_odr hidden constant i16 {{[0-9]+}}

// associated type descriptor for opt_in_reflection_metadata.RefProtocol.RefInner
// CHECK-REL-DAG: @"$s8RefInner26opt_in_reflection_metadata0A8ProtocolPTl" =
// associated type descriptor for opt_in_reflection_metadata.NonRefProtocol.NonRefInner
// CHECK-REL-DAG: @"$s11NonRefInner26opt_in_reflection_metadata0aB8ProtocolPTl" =
