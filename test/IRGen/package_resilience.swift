//
// Tests resilient access is bypassed when package optimization flags are passed.
//
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/package_resilience.swift
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct -allow-non-resilient-access -package-cmo %S/Inputs/package_types/resilient_struct.swift
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/Inputs/package_types/resilient_enum.swift -allow-non-resilient-access -package-cmo
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/Inputs/package_types/resilient_class.swift -allow-non-resilient-access -package-cmo

// RUN: %target-swift-frontend -package-name MyPkg -module-name=package_resilience -enable-objc-interop -I %t -emit-ir -enable-library-evolution %t/package_resilience.swift | %FileCheck %t/package_resilience.swift --check-prefixes=CHECK,CHECK-objc,CHECK-objc%target-ptrsize,CHECK-%target-ptrsize,CHECK-%target-cpu,CHECK-%target-import-type-objc-STABLE-ABI-%target-mandates-stable-abi,CHECK-%target-sdk-name -DINT=i%target-ptrsize -D#MDWORDS=7 -D#MDSIZE32=52 -D#MDSIZE64=80 -D#WORDSIZE=%target-alignment

// RUN: %target-swift-frontend -package-name MyPkg -module-name=package_resilience  -disable-objc-interop -I %t -emit-ir -enable-library-evolution %t/package_resilience.swift | %FileCheck %t/package_resilience.swift --check-prefixes=CHECK,CHECK-native,CHECK-native%target-ptrsize,CHECK-%target-ptrsize,CHECK-%target-cpu,CHECK-native-STABLE-ABI-%target-mandates-stable-abi,CHECK-%target-sdk-name -DINT=i%target-ptrsize -D#MDWORDS=4 -D#MDSIZE32=40 -D#MDSIZE64=56 -D#WORDSIZE=%target-alignment

// RUN: %target-swift-frontend -package-name MyPkg -module-name=package_resilience -experimental-package-bypass-resilience -I %t -emit-ir -enable-library-evolution -O %t/package_resilience.swift -package-name MyPkg

// REQUIRES: objc_codegen
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos
// REQUIRES: CPU=x86_64 || CPU=arm64

// CHECK: @"$s18package_resilience26ClassWithResilientPropertyC1p16resilient_struct5PointVvpWvd" = hidden constant [[INT]] {{8|16}}, align [[#WORDSIZE]]
// CHECK: @"$s18package_resilience26ClassWithResilientPropertyC1s16resilient_struct4SizeVvpWvd" = hidden constant [[INT]] {{32|16}}, align [[#WORDSIZE]]

// CHECK: @"$s18package_resilience33ClassWithResilientlySizedPropertyC1r16resilient_struct9RectangleVvpWvd" = hidden constant [[INT]] {{8|16}}, align [[#WORDSIZE]]
// CHECK: @"$s18package_resilience33ClassWithResilientlySizedPropertyC5colors5Int32VvpWvd" = hidden constant [[INT]] 56, align [[#WORDSIZE]]

// class metadata base offset for package_resilience.MyResilientParent
// CHECK: @"$s18package_resilience17MyResilientParentCMo" = constant [[BOUNDS:{ (i32|i64), i32, i32 }]]
// CHECK-32-SAME: { [[INT]] [[#MDSIZE32]], i32 3, i32 [[#MDWORDS + 6 + 2]] }, align [[#WORDSIZE]]
// CHECK-64-SAME: { [[INT]] [[#MDSIZE64]], i32 3, i32 [[#MDWORDS + 3 + 2]] }, align [[#WORDSIZE]]

// CHECK: @"$s18package_resilience16MyResilientChildC5fields5Int32VvpWvd" = hidden constant [[INT]] [[#WORDSIZE + WORDSIZE + 4]], align [[#WORDSIZE]]

// CHECK: @"$s18package_resilience16MyResilientChildCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-32-SAME: { [[INT]] [[#MDSIZE32 + WORDSIZE + WORDSIZE]], i32 3, i32 [[#MDWORDS + 6 + 3]] }
// CHECK-64-SAME: { [[INT]] [[#MDSIZE64 + WORDSIZE + WORDSIZE]], i32 3, i32 [[#MDWORDS + 3 + 3]] }

// CHECK: @"$s18package_resilience24MyResilientGenericParentCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-32-SAME: { [[INT]] [[#MDSIZE32]], i32 3, i32 [[#MDWORDS + 6 + 3]] }
// CHECK-64-SAME: { [[INT]] [[#MDSIZE64]], i32 3, i32 [[#MDWORDS + 3 + 3]] }

// CHECK: @"$s18package_resilience24MyResilientConcreteChildCMo" = {{(protected )?}}{{(dllexport )?}}constant [[BOUNDS]]
// CHECK-32-SAME: { [[INT]] [[#MDSIZE32 + WORDSIZE + WORDSIZE + WORDSIZE]], i32 3, i32 [[#MDWORDS + 6 + 5]] }
// CHECK-64-SAME: { [[INT]] [[#MDSIZE64 + WORDSIZE + WORDSIZE + WORDSIZE]], i32 3, i32 [[#MDWORDS + 3 + 5]] }

// CHECK: @"$s18package_resilience27ClassWithEmptyThenResilientC5emptyAA0E0VvpWvd" = hidden constant [[INT]] 0,
// CHECK: @"$s18package_resilience27ClassWithEmptyThenResilientC9resilient0H7_struct0G3IntVvpWvd" = hidden constant [[INT]] [[#WORDSIZE + WORDSIZE]], align [[#WORDSIZE]]
// CHECK: @"$s18package_resilience27ClassWithResilientThenEmptyC9resilient0H7_struct0E3IntVvpWvd" = hidden constant [[INT]] [[#WORDSIZE + WORDSIZE]], align [[#WORDSIZE]]
// CHECK: @"$s18package_resilience27ClassWithResilientThenEmptyC5emptyAA0G0VvpWvd" = hidden constant [[INT]] 0,

import resilient_class
import resilient_struct
import resilient_enum

// Concrete class with resilient stored property
package class ClassWithResilientProperty {
  package let p: Point
  package let s: Size
  package let color: Int32

  package init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// Concrete class with non-fixed size stored property

package class ClassWithResilientlySizedProperty {
  package let r: Rectangle
  package let color: Int32

  package init(r: Rectangle, color: Int32) {
    self.r = r
    self.color = color
  }
}


// Concrete class with resilient stored property that
// is fixed-layout inside this resilience domain

package struct MyResilientStruct {
  package let x: Int32
}

package class ClassWithMyResilientProperty {
  package let r: MyResilientStruct
  package let color: Int32

  package init(r: MyResilientStruct, color: Int32) {
    self.r = r
    self.color = color
  }
}


// Enums with indirect payloads are fixed-size

package class ClassWithIndirectResilientEnum {
  package let s: FunnyShape
  package let color: Int32

  package init(s: FunnyShape, color: Int32) {
    self.s = s
    self.color = color
  }
}

// Superclass is resilient and has a resilient value type payload,
// but everything is in one package

package class MyResilientParent {
  package let s: MyResilientStruct = MyResilientStruct(x: 0)
}

package class MyResilientChild : MyResilientParent {
  package let field: Int32 = 0
}

package class MyResilientGenericParent<T> {
  package let t: T

  package init(t: T) {
    self.t = t
  }
}

package class MyResilientConcreteChild : MyResilientGenericParent<Int> {
  package let x: Int

  package init(x: Int) {
    self.x = x
    super.init(t: x)
  }
}

extension ResilientGenericOutsideParent {
  package func genericExtensionMethod() -> A.Type {
    return A.self
  }
}

// rdar://48031465
// Field offsets for empty fields in resilient classes should be initialized
// to their best-known value and made non-constant if that value might
// disagree with the dynamic value.

package struct Empty {}

package class ClassWithEmptyThenResilient {
  package let empty: Empty
  package let resilient: ResilientInt

  package init(empty: Empty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

package class ClassWithResilientThenEmpty {
  package let resilient: ResilientInt
  package let empty: Empty

  package init(empty: Empty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

// ClassWithResilientProperty.color getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$s18package_resilience26ClassWithResilientPropertyC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience26ClassWithResilientPropertyC, ptr %0,
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, ptr [[FIELD_PAYLOAD]]
// CHECK:      ret i32 [[FIELD_VALUE]]

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$s18package_resilience33ClassWithResilientlySizedPropertyC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience33ClassWithResilientlySizedPropertyC, ptr %0,
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, ptr [[FIELD_PAYLOAD]]
// CHECK:      ret i32 [[FIELD_VALUE]]

// ClassWithIndirectResilientEnum.color getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$s18package_resilience30ClassWithIndirectResilientEnumC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience30ClassWithIndirectResilientEnumC, ptr %0, i32 0, i32 2
// CHECK-NEXT: [[FIELD_PAYLOAD:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[FIELD_PTR]], i32 0, i32 0
// CHECK-NEXT: [[FIELD_VALUE:%.*]] = load i32, ptr [[FIELD_PAYLOAD]]
// CHECK-NEXT: ret i32 [[FIELD_VALUE]]

// Make sure that MemoryLayout always emits constants


// MyResilientChild.field getter

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc i32 @"$s18package_resilience16MyResilientChildC5fields5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience16MyResilientChildC, ptr %0, i32 0, i32 2
// CHECK-NEXT: [[PAYLOAD_ADDR:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK-NEXT: [[RESULT:%.*]] = load i32, ptr [[PAYLOAD_ADDR]]
// CHECK:      ret i32 [[RESULT]]

// ResilientGenericOutsideParent.genericExtensionMethod()

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s15resilient_class29ResilientGenericOutsideParentC18package_resilienceE22genericExtensionMethodxmyF"(ptr swiftself %0) {{.*}} {
// CHECK: [[ISA:%.*]] = load ptr, ptr %0
// CHECK:      [[BASE:%.*]] = load [[INT]], ptr @"$s15resilient_class29ResilientGenericOutsideParentCMo"
// CHECK-NEXT: [[GENERIC_PARAM_OFFSET:%.*]] = add [[INT]] [[BASE]], 0
// CHECK-NEXT: [[GENERIC_PARAM_TMP:%.*]] = getelementptr inbounds i8, ptr [[ISA]], [[INT]] [[GENERIC_PARAM_OFFSET]]
// CHECK-NEXT: [[GENERIC_PARAM:%.*]] = load ptr, ptr [[GENERIC_PARAM_TMP]]
// CHECK:       ret ptr [[GENERIC_PARAM]]

// CHECK-LABEL: define{{.*}} swiftcc {{i32|i64}} @"$s18package_resilience38memoryLayoutDotSizeWithResilientStructSiyF"()
public func memoryLayoutDotSizeWithResilientStruct() -> Int {
  // CHECK: ret [[INT]] [[#WORDSIZE + WORDSIZE]]
  return MemoryLayout<Size>.size
}

// CHECK-LABEL: define{{.*}} swiftcc {{i32|i64}} @"$s18package_resilience40memoryLayoutDotStrideWithResilientStructSiyF"()
public func memoryLayoutDotStrideWithResilientStruct() -> Int {
  // CHECK: ret [[INT]] [[#WORDSIZE + WORDSIZE]]
  return MemoryLayout<Size>.stride
}

// CHECK-LABEL: define{{.*}} swiftcc {{i32|i64}} @"$s18package_resilience43memoryLayoutDotAlignmentWithResilientStructSiyF"()
public func memoryLayoutDotAlignmentWithResilientStruct() -> Int {
  // CHECK: ret [[INT]] [[#WORDSIZE]]
  return MemoryLayout<Size>.alignment
}

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void  @"$s18package_resilience31constructResilientEnumNoPayload14resilient_enum6MediumOyF"
package func constructResilientEnumNoPayload() -> Medium {
  // CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %T14resilient_enum6MediumO, ptr %0, i32 0, i32 1
  // CHECK: ret void
  return Medium.Paper
}

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s18package_resilience39constructExhaustiveWithResilientMembers14resilient_enum11SimpleShapeOyF"
package func constructExhaustiveWithResilientMembers() -> SimpleShape {
  // CHECK: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %T14resilient_enum11SimpleShapeO, ptr %0, i32 0, i32 1
  // CHECK: ret void
  return .KleinBottle
}

// ClassWithResilientProperty metadata accessor

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s18package_resilience26ClassWithResilientPropertyCMa"(
// CHECK-objc:      [[METADATA:%.*]] = call ptr {{.*}}@"$s18package_resilience26ClassWithResilientPropertyCMf"{{.*}}
// CHECK-objc-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, ptr [[METADATA]], 0
// CHECK-objc-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], i64 0, 1
// CHECK-objc-NEXT: ret %swift.metadata_response [[T1]]

// CHECK-native: ret %swift.metadata_response

// ClassWithResilientlySizedProperty metadata accessor

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$s18package_resilience33ClassWithResilientlySizedPropertyCMa"(
// CHECK-objc:      [[METADATA:%.*]] = call ptr {{.*}}@"$s18package_resilience33ClassWithResilientlySizedPropertyCMf"{{.*}}
// CHECK-objc-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, ptr [[METADATA]], 0
// CHECK-objc-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], i64 0, 1
// CHECK-objc-NEXT: ret %swift.metadata_response [[T1]]

// CHECK-native: ret %swift.metadata_response


// ClassWithResilientlySizedProperty method lookup function

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s18package_resilience33ClassWithResilientlySizedPropertyCMu"(ptr %0, ptr %1)
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr %0, ptr %1, ptr @"$s18package_resilience33ClassWithResilientlySizedPropertyCMn{{(\.ptrauth.*)?}}")
// CHECK-NEXT:   ret ptr [[RESULT]]
// CHECK-NEXT: }

// ClassWithResilientProperty method lookup function

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s18package_resilience28ClassWithMyResilientPropertyCMu"(ptr %0, ptr %1)
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr %0, ptr %1, ptr @"$s18package_resilience28ClassWithMyResilientPropertyCMn{{(\.ptrauth.*)?}}")
// CHECK-NEXT:   ret ptr [[RESULT]]
// CHECK-NEXT: }
