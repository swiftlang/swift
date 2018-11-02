// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -disable-reflection-names -emit-ir %s | %FileCheck %s --check-prefix=STRIP_REFLECTION_NAMES
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -disable-reflection-metadata -emit-ir %s | %FileCheck %s --check-prefix=STRIP_REFLECTION_METADATA

// STRIP_REFLECTION_NAMES_DAG: section "{{[^"]*swift5_reflect|.sw5rfst\$B}}
// STRIP_REFLECTION_NAMES_DAG: section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// STRIP_REFLECTION_NAMES_DAG: section "{{[^"]*swift5_assocty|.sw5asty\$B}}
// STRIP_REFLECTION_NAMES-DAG: section "{{[^"]*swift5_capture|.sw5cptr\$B}}
// STRIP_REFLECTION_NAMES-DAG: section "{{[^"]*swift5_typeref|.sw5tyrf\$B}}
// STRIP_REFLECTION_NAMES-NOT: section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// STRIP_REFLECTION_NAMES-NOT: section "{{[^"]*swift5_builtin|.sw5bltn\$B}}

// STRIP_REFLECTION_NAMES-DAG: @"$S19reflection_metadata10MyProtocol_pMF" = internal constant {{.*}}swift5_fieldmd

// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_reflect|.sw5rfst\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_assocty|.sw5asty\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_capture|.sw5cptr\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_typeref|.sw5tyrf\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// STRIP_REFLECTION_METADATA-NOT: section "{{[^"]*swift5_builtin|.sw5bltn\$B}}

// CHECK-DAG: @__swift_reflection_version = linkonce_odr hidden constant i16 {{[0-9]+}}
// CHECK-DAG: private constant [2 x i8] c"i\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"ms\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"me\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"mc\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [2 x i8] c"C\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [2 x i8] c"S\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [2 x i8] c"E\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [2 x i8] c"I\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [2 x i8] c"t\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [4 x i8] c"mgs\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [4 x i8] c"mge\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [4 x i8] c"mgc\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"GC\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"GS\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}
// CHECK-DAG: private constant [3 x i8] c"GE\00", section "{{[^"]*swift5_reflstr|.sw5rfst\$B}}

// CHECK-DAG: @"\01l__swift5_reflection_descriptor" = private constant { {{.*}} } { i32 1, i32 1, i32 2, {{.*}} }

// CHECK-DAG: @"$S19reflection_metadata10MyProtocol_pMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata7MyClassCMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata11ConformanceVAA10MyProtocolAAMA" = internal constant {{.*}} section "{{[^"]*swift5_assocty|.sw5asty\$B}}
// CHECK-DAG: @"$S19reflection_metadata8MyStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata6MyEnumOMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata14MyGenericClassCMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata15MyGenericStructVMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}
// CHECK-DAG: @"$S19reflection_metadata13MyGenericEnumOMF" = internal constant {{.*}} section "{{[^"]*swift5_fieldmd|.sw5flmd\$B}}

public protocol MyProtocol {
  associatedtype Inner
  var inner: Inner { get }
}

public class MyClass {
  let i: Int
  let ms: MyStruct
  let me: MyEnum
  public init(i: Int, ms: MyStruct, me: MyEnum) {
    self.i = i
    self.ms = ms
    self.me = me
  }
}

public struct Conformance : MyProtocol {
  public var inner: Int = 0
}

public struct MyStruct {
  let i: Int
  let mc: MyClass
  let me: MyEnum
}

public enum MyEnum {
  case C(MyClass)
  indirect case S(MyStruct)
  indirect case E(MyEnum)
  case I(Int)
}

public class MyGenericClass<T : MyProtocol> {
  let t: T
  let i: T.Inner
  let mgs: MyGenericStruct<T>
  let mge: MyGenericEnum<T>

  public init(t: T, i: T.Inner, mgs: MyGenericStruct<T>, mge: MyGenericEnum<T>) {
    self.t = t
    self.i = i
    self.mgs = mgs
    self.mge = mge
  }
}

public struct MyGenericStruct<T : MyProtocol> {
  let t: T
  let i: T.Inner
  let mgc: MyGenericClass<T>
  let mge: MyGenericEnum<T>
}

public enum MyGenericEnum<T : MyProtocol> {
  case GC(MyGenericClass<T>)
  indirect case GS(MyGenericStruct<T>)
  indirect case GE(MyGenericEnum<T>)
  case I(Int)
}

public func makeSomeClosures<T : MyProtocol>(t: T) -> (() -> ()) {
  return { _ = t }
}
