// This verifies package decls are treated as resilient as (non-frozen) public
// decls by default. When non-resilience optimization is enabled, references
// to package decls at a use site can be treated as non-resilient, which is
// verified in test/IRGen/package_resilience.swift.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/package_resilience.swift
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/Inputs/package_types/resilient_struct.swift
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/Inputs/package_types/resilient_enum.swift
// RUN: %target-swift-frontend -package-name MyPkg -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/Inputs/package_types/resilient_class.swift

// RUN: %target-swift-frontend -module-name package_resilience -package-name MyPkg -I %t -emit-ir -o %t/ir-result.ll -enable-library-evolution %t/package_resilience.swift
// RUN: %FileCheck %t/package_resilience.swift --check-prefixes=CHECK -DINT=i%target-ptrsize -D#MDWORDS=7 -D#MDSIZE32=52 -D#MDSIZE64=80 -D#WORDSIZE=%target-alignment < %t/ir-result.ll

// RUN: %target-swift-frontend -module-name package_resilience -package-name MyPkg -I %t -emit-ir -enable-library-evolution -O %t/package_resilience.swift -package-name MyPkg
// REQUIRES: objc_codegen
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos
// REQUIRES: CPU=x86_64 || CPU=arm64

// CHECK: @"$s18package_resilience27PublicClassWithPbPropertiesC1p16resilient_struct0C5PointVvpWvd" = hidden global
// CHECK: @"$s18package_resilience27PublicClassWithPbPropertiesC1s16resilient_struct0C4SizeVvpWvd" = hidden global
// CHECK: @"$s18package_resilience27PublicClassWithPbPropertiesC5colors5Int32VvpWvd" = hidden global
// CHECK: @"$s18package_resilience28PkgClassWithPublicPropertiesC1p16resilient_struct0F5PointVvpWvd" = hidden global
// CHECK: @"$s18package_resilience28PkgClassWithPublicPropertiesC1s16resilient_struct0F4SizeVvpWvd" = hidden global
// CHECK: @"$s18package_resilience28PkgClassWithPublicPropertiesC5colors5Int32VvpWvd" = hidden global
// CHECK: @"$s18package_resilience24PkgWithPackagePropertiesC1p16resilient_struct5PointVvpWvd" = hidden global
// CHECK: @"$s18package_resilience24PkgWithPackagePropertiesC1s16resilient_struct4SizeVvpWvd" = hidden global
// CHECK: @"$s18package_resilience24PkgWithPackagePropertiesC5colors5Int32VvpWvd" = hidden global 
// CHECK: @"$s18package_resilience32PublicClassWithEnumIndirectCasesC1s14resilient_enum0C10FunnyShapeOvpWvd" = hidden global
// CHECK: @"$s18package_resilience32PublicClassWithEnumIndirectCasesC5colors5Int32VvpWvd" = hidden global 
// CHECK: @"$s18package_resilience29PkgClassWithEnumIndirectCasesC1s14resilient_enum10FunnyShapeOvpWvd" = hidden global
// CHECK: @"$s18package_resilience29PkgClassWithEnumIndirectCasesC5colors5Int32VvpWvd" = hidden global 

// CHECK: @"$s18package_resilience33PublicClassWithEmptyThenResilientC9resilient0I7_struct0cH3IntVvpWvd" = hidden global
// CHECK: @"$s18package_resilience33PublicClassWithResilientThenEmptyC9resilient0I7_struct0cF3IntVvpWvd" = hidden global
// CHECK: @"$s18package_resilience30PkgClassWithEmptyThenResilientC9resilient0I7_struct0H3IntVvpWvd" = hidden global
// CHECK: @"$s18package_resilience30PkgClassWithResilientThenEmptyC9resilient0I7_struct0F3IntVvpWvd" = hidden global

// CHECK: @"$s18package_resilience33PublicClassWithFrozenPbPropertiesC1p16resilient_struct0fC5PointVvpWvd" = hidden constant
// CHECK: @"$s18package_resilience33PublicClassWithFrozenPbPropertiesC1s16resilient_struct0fC4SizeVvpWvd" = hidden constant
// CHECK: @"$s18package_resilience33PublicClassWithFrozenPbPropertiesC5colors5Int32VvpWvd" = hidden constant

// CHECK: @"$s18package_resilience17PublicClassParentC1sAA0C12StructSimpleVvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience16PublicClassChildC5fields5Int32VvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience24PublicClassConcreteChildC1xSivpWvd" = hidden constant 
// CHECK: @"$s18package_resilience14PkgClassParentC1sAA0C12StructSimpleVvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience13PkgClassChildC5fields5Int32VvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience21PkgClassConcreteChildC1xSivpWvd" = hidden constant 

// CHECK: @"$s18package_resilience33PublicClassWithEmptyThenResilientC5emptyAA0cF0VvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience33PublicClassWithResilientThenEmptyC5emptyAA0cH0VvpWvd" = hidden constant 
// CHECK: @"$s18package_resilience30PkgClassWithEmptyThenResilientC5emptyAA0c6StructF0VvpWvd" = hidden constant
// CHECK: @"$s18package_resilience30PkgClassWithResilientThenEmptyC5emptyAA0c6StructH0VvpWvd" = hidden constant

import resilient_class
import resilient_struct
import resilient_enum

public class PublicClassWithPbProperties {
  public let p: PublicPoint
  public let s: PublicSize
  public let color: Int32

  public init(p: PublicPoint, s: PublicSize, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// PublicClassWithPbProperties.p getter
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s18package_resilience27PublicClassWithPbPropertiesC1p16resilient_struct0C5PointVvg"(ptr noalias sret(%swift.opaque) %0, ptr swiftself %1)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience27PublicClassWithPbPropertiesC1p16resilient_struct0C5PointVvpWvd"
// CHECK-NEXT:  [[FIELD_PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT:  [[A:%.*]] = call swiftcc %swift.metadata_response @"$s16resilient_struct11PublicPointVMa"
// CHECK-NEXT:  [[B:%.*]] = extractvalue %swift.metadata_response [[A]], 0
// CHECK-NEXT:  [[C:%.*]] = getelementptr inbounds ptr, ptr [[B]],
// CHECK-NEXT:  [[FIELD_VW:%.*]] = load ptr, ptr [[C]]
// CHECK-NEXT:  [[D:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VW]]
// CHECK-NEXT:  [[FIELD_PAYLOAD:%.*]] = load ptr, ptr [[D]]
// CHECK-NEXT:  [[E:%.*]] = call ptr [[FIELD_PAYLOAD]](ptr noalias
// CHECK-NEXT:  ret void

// PublicClassWithPbProperties.color getter

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience27PublicClassWithPbPropertiesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience27PublicClassWithPbPropertiesC5colors5Int32VvpWvd"
// CHECK-NEXT:  [[PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT:  [[VALUE:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[PTR]]
// CHECK-NEXT:  [[RET:%.*]] = load {{i32|i64}}, ptr [[VALUE]]
// CHECK-NEXT:  ret {{i32|i64}} [[RET]]




// PkgClassWithPublicProperties.p getter

// CHECK: define{{.*}} swiftcc void @"$s18package_resilience28PkgClassWithPublicPropertiesC1p16resilient_struct0F5PointVvg"(ptr noalias sret(%swift.opaque) %0, ptr swiftself %1)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience28PkgClassWithPublicPropertiesC1p16resilient_struct0F5PointVvpWvd"
// CHECK-NEXT:  [[FIELD_PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT:  [[A:%.*]] = call swiftcc %swift.metadata_response @"$s16resilient_struct11PublicPointVMa"(i64 0)
// CHECK-NEXT:  [[B:%.*]] = extractvalue %swift.metadata_response [[A]], 0
// CHECK-NEXT:  [[C:%.*]] = getelementptr inbounds ptr, ptr [[B]]
// CHECK-NEXT:  [[FIELD_VW:%.*]] = load ptr, ptr [[C]]
// CHECK-NEXT:  [[D:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VW]]
// CHECK-NEXT:  [[FIELD_PAYLOAD:%.*]] = load ptr, ptr [[D]]
// CHECK-NEXT:  [[E:%.*]] = call ptr [[FIELD_PAYLOAD]](ptr noalias
// CHECK-NEXT:  ret void

package class PkgClassWithPublicProperties {
  package let p: PublicPoint
  package let s: PublicSize
  package let color: Int32

  package init(p: PublicPoint, s: PublicSize, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// PkgClassWithPublicProperties.color getter

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience28PkgClassWithPublicPropertiesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience28PkgClassWithPublicPropertiesC5colors5Int32VvpWvd"
// CHECK-NEXT:  [[PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT:  [[VALUE:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[PTR]]
// CHECK-NEXT:  [[RET:%.*]] = load {{i32|i64}}, ptr [[VALUE]]
// CHECK-NEXT:  ret {{i32|i64}} [[RET]]

package class PkgWithPackageProperties {
  package let p: Point
  package let s: Size
  package let color: Int32

  package init(p: Point, s: Size, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// PkgWithPackageProperties.p getter

// CHECK: define{{.*}} swiftcc void @"$s18package_resilience24PkgWithPackagePropertiesC1p16resilient_struct5PointVvg"(ptr noalias sret(%swift.opaque) %0, ptr swiftself %1)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience24PkgWithPackagePropertiesC1p16resilient_struct5PointVvpWvd"
// CHECK:       [[FIELD_PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, i64 [[OFFSET]]
// CHECK-NEXT:  [[A:%.*]] = call swiftcc %swift.metadata_response @"$s16resilient_struct5PointVMa"
// CHECK-NEXT:  [[B:%.*]] = extractvalue %swift.metadata_response [[A]], 0
// CHECK-NEXT:  [[C:%.*]] = getelementptr inbounds ptr, ptr [[B]]
// CHECK-NEXT:  [[FIELD_VW:%.*]] = load ptr, ptr [[C]]
// CHECK-NEXT:  [[D:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VW]]
// CHECK-NEXT:  [[FIELD_PAYLOAD:%.*]] = load ptr, ptr [[D]]
// CHECK-NEXT:  [[E:%.*]] = call ptr [[FIELD_PAYLOAD]](ptr noalias
// CHECK-NEXT:  ret void

// PkgWithPackageProperties.color getter

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience24PkgWithPackagePropertiesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:       [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience24PkgWithPackagePropertiesC5colors5Int32VvpWvd"
// CHECK-NEXT:  [[PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT:  [[VALUE:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[PTR]]
// CHECK-NEXT:  [[RET:%.*]] = load {{i32|i64}}, ptr [[VALUE]]
// CHECK-NEXT:  ret {{i32|i64}} [[RET]]


public class PublicClassWithFrozenPbProperties {
  public let p: FrozenPublicPoint
  public let s: FrozenPublicSize
  public let color: Int32

  public init(p: FrozenPublicPoint, s: FrozenPublicSize, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}

// PublicClassWithFrozenPbProperties.p getter

// CHECK: define{{.*}} swiftcc { {{i32|i64}}, {{i32|i64}} } @"$s18package_resilience33PublicClassWithFrozenPbPropertiesC1p16resilient_struct0fC5PointVvg"(ptr swiftself %0)
// CHECK:      [[T1:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience33PublicClassWithFrozenPbPropertiesC, ptr {{.*}}
// CHECK-NEXT: [[X:%.*]] = getelementptr inbounds{{.*}} %T16resilient_struct17FrozenPublicPointV, ptr [[T1]]
// CHECK-NEXT: [[XVAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[X]]
// CHECK-NEXT: [[T2:%.*]] = load {{i32|i64}}, ptr [[XVAL]]
// CHECK-NEXT: [[Y:%.*]] = getelementptr inbounds{{.*}} %T16resilient_struct17FrozenPublicPointV, ptr [[T1]]
// CHECK-NEXT: [[YVAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[Y]]
// CHECK-NEXT: [[T3:%.*]] = load {{i32|i64}}, ptr [[YVAL]]
// CHECK-NEXT: [[T4:%.*]] = insertvalue {{.*}} undef, {{i32|i64}} [[T2]], 0
// CHECK-NEXT: [[T5:%.*]] = insertvalue {{.*}} [[T4]], {{i32|i64}} [[T3]], 1
// CHECK-NEXT: ret { {{i32|i64}}, {{i32|i64}} } [[T5]]


// PublicClassWithFrozenPbProperties.color getter

// CHECK: define{{.*}} swiftcc {{i32|64}} @"$s18package_resilience33PublicClassWithFrozenPbPropertiesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[T1:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience33PublicClassWithFrozenPbPropertiesC, ptr {{.*}}
// CHECK-NEXT: [[VAL:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[T1]]
// CHECK-NEXT: [[T2:%.*]] = load {{i32|i64}}, ptr [[VAL]]
// CHECK-NEXT: ret {{i32|i64}} [[T2]]


package class PkgClassWithFrozenPublicProperties {
  package let p: FrozenPublicPoint
  package let s: FrozenPublicSize
  package let color: Int32

  package init(p: FrozenPublicPoint, s: FrozenPublicSize, color: Int32) {
    self.p = p
    self.s = s
    self.color = color
  }
}


// PkgClassWithFrozenPublicProperties.p getter

// CHECK: define{{.*}} swiftcc { i64, i64 } @"$s18package_resilience34PkgClassWithFrozenPublicPropertiesC1p16resilient_struct0fG5PointVvg"(ptr swiftself %0)
// CHECK:      [[T1:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience34PkgClassWithFrozenPublicPropertiesC, ptr {{.*}}
// CHECK-NEXT: [[X:%.*]] = getelementptr inbounds{{.*}} %T16resilient_struct17FrozenPublicPointV, ptr [[T1]]
// CHECK-NEXT: [[XVAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[X]]
// CHECK-NEXT: [[T2:%.*]] = load {{i32|i64}}, ptr [[XVAL]]
// CHECK-NEXT: [[Y:%.*]] = getelementptr inbounds{{.*}} %T16resilient_struct17FrozenPublicPointV, ptr [[T1]]
// CHECK-NEXT: [[YVAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[Y]]
// CHECK-NEXT: [[T3:%.*]] = load {{i32|i64}}, ptr [[YVAL]]
// CHECK-NEXT: [[T4:%.*]] = insertvalue { i64, i64 } undef, i64 [[T2]], 0
// CHECK-NEXT: [[T5:%.*]] = insertvalue { i64, i64 } %4, i64 [[T3]], 1
// CHECK-NEXT: ret { {{i32|i64}}, {{i32|i64}} } [[T5]]

// PkgClassWithFrozenPublicProperties.color getter

// CHECK: define{{.*}} swiftcc {{i32|64}} @"$s18package_resilience34PkgClassWithFrozenPublicPropertiesC5colors5Int32Vvg"(ptr swiftself %0) #0 {
// CHECK:      [[T1:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience34PkgClassWithFrozenPublicPropertiesC, ptr {{.*}}
// CHECK-NEXT: [[VAL:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[T1]]
// CHECK-NEXT: [[T2:%.*]] = load {{i32|i64}}, ptr [[VAL]]
// CHECK-NEXT: ret {{i32|i64}} [[T2]]


// Enums with indirect payloads are fixed-size
public class PublicClassWithEnumIndirectCases {
  public let s: PublicFunnyShape
  public let color: Int32

  public init(s: PublicFunnyShape, color: Int32) {
    self.s = s
    self.color = color
  }
}

// PublicClassWithEnumIndirectCases.s getter

// CHECK: define{{.*}} swiftcc void @"$s18package_resilience32PublicClassWithEnumIndirectCasesC1s14resilient_enum0C10FunnyShapeOvg"(ptr noalias sret(%swift.opaque) %0, ptr swiftself %1)
// CHECK:      [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience32PublicClassWithEnumIndirectCasesC1s14resilient_enum0C10FunnyShapeOvpWvd"
// CHECK-NEXT: [[A:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT: [[B:%.*]] = call swiftcc %swift.metadata_response @"$s14resilient_enum16PublicFunnyShapeOMa"
// CHECK-NEXT: [[C:%.*]] = extractvalue %swift.metadata_response [[B]], 0
// CHECK-NEXT: [[D:%.*]] = getelementptr inbounds ptr, ptr [[C]]
// CHECK-NEXT: [[VW:%.*]] = load ptr, ptr [[D]]
// CHECK-NEXT: [[E:%.*]] = getelementptr inbounds ptr, ptr [[VW]]
// CHECK-NEXT: [[PAYLOAD:%.*]] = load ptr, ptr [[E]]
// CHECK-NEXT: [[RET:%.*]] = call ptr [[PAYLOAD]](ptr noalias
// CHECK-NEXT: ret void

// PublicClassWithEnumIndirectCases.color getter

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc {{.*}} @"$s18package_resilience32PublicClassWithEnumIndirectCasesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience32PublicClassWithEnumIndirectCasesC5colors5Int32VvpWvd",
// CHECK-NEXT: [[PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT: [[VALUE:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[PTR]]
// CHECK-NEXT: [[RET:%.*]] = load {{i32|i64}}, ptr [[VALUE]]
// CHECK-NEXT: ret {{i32|i64}} [[RET]]

package class PkgClassWithEnumIndirectCases {
  package let s: FunnyShape
  package let color: Int32

  package init(s: FunnyShape, color: Int32) {
    self.s = s
    self.color = color
  }
}

// PkgClassWithEnumIndirectCases.s getter

// CHECK: define{{.*}} swiftcc void @"$s18package_resilience29PkgClassWithEnumIndirectCasesC1s14resilient_enum10FunnyShapeOvg"(ptr noalias sret(%swift.opaque) %0, ptr swiftself %1)
// CHECK:      [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience29PkgClassWithEnumIndirectCasesC1s14resilient_enum10FunnyShapeOvpWvd"
// CHECK-NEXT: [[A:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT: [[B:%.*]] = call swiftcc %swift.metadata_response @"$s14resilient_enum10FunnyShapeOMa"
// CHECK-NEXT: [[C:%.*]] = extractvalue %swift.metadata_response [[B]], 0
// CHECK-NEXT: [[D:%.*]] = getelementptr inbounds ptr, ptr [[C]]
// CHECK-NEXT: [[VW:%.*]] = load ptr, ptr [[D]]
// CHECK-NEXT: [[E:%.*]] = getelementptr inbounds ptr, ptr [[VW]]
// CHECK-NEXT: [[PAYLOAD:%.*]] = load ptr, ptr [[E]]
// CHECK-NEXT: [[RET:%.*]] = call ptr [[PAYLOAD]](ptr noalias
// CHECK-NEXT: ret void

// PkgClassWithEnumIndirectCases.color getter

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc {{i32|64}} @"$s18package_resilience29PkgClassWithEnumIndirectCasesC5colors5Int32Vvg"(ptr swiftself %0)
// CHECK:      [[OFFSET:%.*]] = load {{i32|i64}}, ptr @"$s18package_resilience29PkgClassWithEnumIndirectCasesC5colors5Int32VvpWvd",
// CHECK-NEXT: [[PTR:%.*]] = getelementptr inbounds i8, ptr {{.*}}, {{i32|i64}} [[OFFSET]]
// CHECK-NEXT: [[VALUE:%.*]] = getelementptr inbounds{{.*}} %Ts5Int32V, ptr [[PTR]]
// CHECK-NEXT: [[RET:%.*]] = load {{i32|i64}}, ptr [[VALUE]]
// CHECK-NEXT: ret {{i32|i64}} [[RET]]


public struct PublicStructSimple {
  public let x: Int32
}

public class PublicClassParent {
  public let s: PublicStructSimple = PublicStructSimple(x: 0)
}

public class PublicClassChild : PublicClassParent {
  public let field: Int32 = 0
}

public class PublicClassGenericParent<T> {
  package let t: T

  package init(t: T) {
    self.t = t
  }
}

public class PublicClassConcreteChild : PublicClassGenericParent<Int> {
  public let x: Int

  public init(x: Int) {
    self.x = x
    super.init(t: x)
  }
}

// PublicClassConcreteChild.x getter

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc {{.*}} @"$s18package_resilience24PublicClassConcreteChildC1xSivg"(ptr swiftself %0)
// CHECK:      [[PTR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience24PublicClassConcreteChildC, ptr {{.*}}
// CHECK-NEXT: [[VAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[PTR]]
// CHECK-NEXT: [[RET:%.*]] = load {{i32|i64}}, ptr [[VAL]]
// CHECK-NEXT: ret {{i32|i64}} [[RET]]



package struct PkgStructSimple {
  package let x: Int32
}

package class PkgClassParent {
  package let s: PkgStructSimple = PkgStructSimple(x: 0)
}

package class PkgClassChild : PkgClassParent {
  package let field: Int32 = 0
}

package class PkgClassGenericParent<T> {
  package let t: T

  package init(t: T) {
    self.t = t
  }
}

package class PkgClassConcreteChild : PkgClassGenericParent<Int> {
  package let x: Int

  package init(x: Int) {
    self.x = x
    super.init(t: x)
  }
}


// PkgClassConcreteChild.x getter

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc {{.*}} @"$s18package_resilience21PkgClassConcreteChildC1xSivg"(ptr swiftself %0)
// CHECK:      [[PTR:%.*]] = getelementptr inbounds{{.*}} %T18package_resilience21PkgClassConcreteChildC, ptr {{.*}}
// CHECK-NEXT: [[VAL:%.*]] = getelementptr inbounds{{.*}} %TSi, ptr [[PTR]]
// CHECK-NEXT: [[RET:%.*]] = load {{i32|i64}}, ptr [[VAL]]
// CHECK-NEXT: ret {{i32|i64}} [[RET]]



extension PublicGenericOutsideParent {
  public func publicGenericExtensionMethod() -> A.Type {
    return A.self
  }
}

// PublicGenericOutsideParent.publicGenericExtensionMethod()

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s15resilient_class26PublicGenericOutsideParentC18package_resilienceE06publicD15ExtensionMethodxmyF"(ptr swiftself %0)
// CHECK: [[ISA:%.*]] = load ptr, ptr {{.*}}
// CHECK:      [[BASE:%.*]] = load {{i32|i64}}, ptr @"$s15resilient_class26PublicGenericOutsideParentCMo"
// CHECK-NEXT: [[GENERIC_PARAM_OFFSET:%.*]] = add {{i32|i64}} [[BASE]], 0
// CHECK-NEXT: [[GENERIC_PARAM_TMP:%.*]] = getelementptr inbounds i8, ptr [[ISA]], {{i32|i64}} [[GENERIC_PARAM_OFFSET]]
// CHECK-NEXT: [[GENERIC_PARAM:%.*]] = load ptr, ptr [[GENERIC_PARAM_TMP]]

extension ResilientGenericOutsideParent {
  package func pkgGenericExtensionMethod() -> A.Type {
    return A.self
  }
}

// ResilientGenericOutsideParent.pkgGenericExtensionMethod()

// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s15resilient_class29ResilientGenericOutsideParentC18package_resilienceE03pkgD15ExtensionMethodxmyF"(ptr swiftself %0)
// CHECK: [[ISA:%.*]] = load ptr, ptr {{.*}}
// CHECK:      [[BASE:%.*]] = load {{i32|i64}}, ptr @"$s15resilient_class29ResilientGenericOutsideParentCMo"
// CHECK-NEXT: [[GENERIC_PARAM_OFFSET:%.*]] = add {{i32|i64}} [[BASE]], 0
// CHECK-NEXT: [[GENERIC_PARAM_TMP:%.*]] = getelementptr inbounds i8, ptr [[ISA]], {{i32|i64}} [[GENERIC_PARAM_OFFSET]]
// CHECK-NEXT: [[GENERIC_PARAM:%.*]] = load ptr, ptr [[GENERIC_PARAM_TMP]]



// rdar://48031465
// Field offsets for empty fields in resilient classes should be initialized
// to their best-known value and made non-constant if that value might
// disagree with the dynamic value.

public struct PublicEmpty {}

public class PublicClassWithEmptyThenResilient {
  public let empty: PublicEmpty
  public let resilient: PublicResilientInt

  public init(empty: PublicEmpty, resilient: PublicResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

public class PublicClassWithResilientThenEmpty {
  public let resilient: PublicResilientInt
  public let empty: PublicEmpty

  public init(empty: PublicEmpty, resilient: PublicResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

package struct PkgStructEmpty {}

package class PkgClassWithEmptyThenResilient {
  package let empty: PkgStructEmpty
  package let resilient: ResilientInt

  package init(empty: PkgStructEmpty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

package class PkgClassWithResilientThenEmpty {
  package let resilient: ResilientInt
  package let empty: PkgStructEmpty

  package init(empty: PkgStructEmpty, resilient: ResilientInt) {
    self.empty = empty
    self.resilient = resilient
  }
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience35memoryLayoutDotSizeWithPublicStructSiyF"()
public func memoryLayoutDotSizeWithPublicStruct() -> Int {
  // CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
  // CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
  // CHECK-NEXT: [[SIZE:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
  // CHECK-NEXT: ret {{i32|i64}} [[SIZE]]
  return MemoryLayout<PublicSize>.size
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience32memoryLayoutDotSizeWithPkgStructSiyF"()
package func memoryLayoutDotSizeWithPkgStruct() -> Int {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[SIZE:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
// CHECK-NEXT: ret {{i32|i64}} [[SIZE]]
  return MemoryLayout<Size>.size
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience41memoryLayoutDotSizeWithFrozenPublicStructSiyF"()
public func memoryLayoutDotSizeWithFrozenPublicStruct() -> Int {
// CHECK-NEXT: entry:
// CHECK-NEXT: ret {{i32|i64}} {{.*}}
  return MemoryLayout<FrozenPublicSize>.size
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience37memoryLayoutDotStrideWithPublicStructSiyF"()
public func memoryLayoutDotStrideWithPublicStruct() -> Int {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[STRIDE:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
// CHECK-NEXT: ret {{i32|i64}} [[STRIDE]]
  return MemoryLayout<PublicSize>.stride
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience34memoryLayoutDotStrideWithPkgStructSiyF"()
package func memoryLayoutDotStrideWithPkgStruct() -> Int {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[STRIDE:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
// CHECK-NEXT: ret {{i32|i64}} [[STRIDE]]
  return MemoryLayout<Size>.stride
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience43memoryLayoutDotStrideWithFrozenPublicStructSiyF"()
public func memoryLayoutDotStrideWithFrozenPublicStruct() -> Int {
// CHECK-NEXT: entry:
// CHECK-NEXT: ret {{i32|i64}} {{.*}}
  return MemoryLayout<FrozenPublicSize>.stride
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience40memoryLayoutDotAlignmentWithPublicStructSiyF"()
public func memoryLayoutDotAlignmentWithPublicStruct() -> Int {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
// CHECK-NEXT:  [[FIELD_FLAGS:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
// CHECK-NEXT:  [[FIELD_ADDR:%.*]] = zext {{i32|i64}} [[FIELD_FLAGS]] to {{i32|i64}}
// CHECK-NEXT:  [[FIELD_MASK:%.*]] = and {{i32|i64}} [[FIELD_ADDR]]
// CHECK-NEXT:  [[FIELD_PAYLOAD:%.*]] = add {{i32|i64}} [[FIELD_MASK]], 1
// CHECK-NEXT:  ret {{i32|i64}} [[FIELD_PAYLOAD]]
  return MemoryLayout<PublicSize>.alignment
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience43memoryLayoutDotAlignmentWithResilientStructSiyF"()
package func memoryLayoutDotAlignmentWithResilientStruct() -> Int {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds{{.*}} %swift.vwtable, ptr [[FIELD_VALUE]],
// CHECK-NEXT:  [[FIELD_FLAGS:%.*]] = load {{i32|i64}}, ptr [[FIELD_PTR]],
// CHECK-NEXT:  [[FIELD_ADDR:%.*]] = zext {{i32|i64}} [[FIELD_FLAGS]] to {{i32|i64}}
// CHECK-NEXT:  [[FIELD_MASK:%.*]] = and {{i32|i64}} [[FIELD_ADDR]]
// CHECK-NEXT:  [[FIELD_PAYLOAD:%.*]] = add {{i32|i64}} [[FIELD_MASK]], 1
// CHECK-NEXT:  ret {{i32|i64}} [[FIELD_PAYLOAD]]
  return MemoryLayout<Size>.alignment
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience46memoryLayoutDotAlignmentWithFrozenPublicStructSiyF"()
public func memoryLayoutDotAlignmentWithFrozenPublicStruct() -> Int {
// CHECK-NEXT: entry:
// CHECK-NEXT: ret {{i32|i64}} {{.*}}
  return MemoryLayout<FrozenPublicSize>.alignment
}


// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience37constructPublicResilientEnumNoPayload14resilient_enum0D6MediumOyF"(ptr noalias
public func constructPublicResilientEnumNoPayload() -> PublicMedium {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[TAG:%.*]] = load ptr, ptr [[FIELD_PTR]],
// CHECK-NEXT: call void [[TAG]]
// CHECK-NEXT: ret void
  return PublicMedium.Paper
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience34constructPkgResilientEnumNoPayload14resilient_enum6MediumOyF"
package func constructPkgResilientEnumNoPayload() -> Medium {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[TAG:%.*]] = load ptr, ptr [[FIELD_PTR]],
// CHECK-NEXT: call void [[TAG]]
// CHECK-NEXT: ret void
  return Medium.Paper
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience43pub_constructExhaustiveWithResilientMembers14resilient_enum17PublicSimpleShapeOyF"
public func pub_constructExhaustiveWithResilientMembers() -> PublicSimpleShape {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[TAG:%.*]] = load ptr, ptr [[FIELD_PTR]],
// CHECK-NEXT: call void [[TAG]]
// CHECK-NEXT: ret void
  return .pbKleinBottle
}

// CHECK: define{{.*}} swiftcc {{.*}} @"$s18package_resilience43pkg_constructExhaustiveWithResilientMembers14resilient_enum11SimpleShapeOyF"
package func pkg_constructExhaustiveWithResilientMembers() -> SimpleShape {
// CHECK: [[FIELD_VALUE:%.*]] = load ptr, ptr {{.*}},
// CHECK-NEXT: [[FIELD_PTR:%.*]] = getelementptr inbounds ptr, ptr [[FIELD_VALUE]],
// CHECK-NEXT: [[TAG:%.*]] = load ptr, ptr [[FIELD_PTR]],
// CHECK-NEXT: call void [[TAG]]
// CHECK-NEXT: ret void
  return .KleinBottle
}


// PublicClassWithPbProperties metadata accessor
//3535
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s18package_resilience27PublicClassWithPbPropertiesCMa"
// CHECK-NEXT: entry:
// CHECK-NEXT: [[PTR:%.*]] = load ptr, ptr @"$s18package_resilience27PublicClassWithPbPropertiesCMl"
// CHECK-NEXT: [[RET:%.*]] = icmp eq ptr [[PTR]]
// CHECK-NEXT: br i1 [[RET]], label %cacheIsNull, label %cont



// PublicClassWithPbProperties method lookup function

// 3606
// CHECK: define{{.*}} swiftcc ptr @"$s18package_resilience27PublicClassWithPbPropertiesCMu"(ptr {{.*}}, ptr {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr {{.*}}, ptr {{.*}}, ptr @"$s18package_resilience27PublicClassWithPbPropertiesCMn")
// CHECK-NEXT:   ret ptr [[RESULT]]



// PkgClassWithPublicProperties metadata accessor
//3630
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s18package_resilience28PkgClassWithPublicPropertiesCMa"
// CHECK:      [[T1:%.*]] = load ptr, ptr @"$s18package_resilience28PkgClassWithPublicPropertiesCMl"
// CHECK-NEXT: [[T2:%.*]] = icmp eq ptr [[T1]], null
// CHECK-NEXT: br i1 [[T2]], label %cacheIsNull, label %cont



// PkgClassWithPublicProperties method lookup
// 3701
// CHECK: define{{.*}} swiftcc ptr @"$s18package_resilience28PkgClassWithPublicPropertiesCMu"(ptr {{.*}}, ptr {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr {{.*}}, ptr {{.*}}, ptr @"$s18package_resilience28PkgClassWithPublicPropertiesCMn")
// CHECK-NEXT:   ret ptr [[RESULT]]


// PkgWithPackageProperties metadata accessor
//3716
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s18package_resilience24PkgWithPackagePropertiesCMa"
// CHECK:      [[T1:%.*]] = load ptr, ptr @"$s18package_resilience24PkgWithPackagePropertiesCMl"
// CHECK-NEXT: [[T2:%.*]] = icmp eq ptr [[T1]], null
// CHECK-NEXT: br i1 [[T2]], label %cacheIsNull, label %cont


// PkgWithPackageProperties method lookup
//3787
// CHECK: define{{.*}} swiftcc ptr @"$s18package_resilience24PkgWithPackagePropertiesCMu"(ptr {{.*}}, ptr {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr {{.*}}, ptr {{.*}}, ptr @"$s18package_resilience24PkgWithPackagePropertiesCMn")
// CHECK-NEXT:   ret ptr [[RESULT]]

// PublicClassWithFrozenPbProperties metadata accessor
//3802
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s18package_resilience33PublicClassWithFrozenPbPropertiesCMa"
// CHECK:      [[A:%.*]] = insertvalue %swift.metadata_response undef, ptr {{.*}}, 0
// CHECK-NEXT: [[B:%.*]] = insertvalue %swift.metadata_response [[A]]
// CHECK-NEXT: ret %swift.metadata_response [[B]]



// PublicClassWithFrozenPbProperties method lookup
//3810
// CHECK: define{{.*}} swiftcc ptr @"$s18package_resilience33PublicClassWithFrozenPbPropertiesCMu"(ptr {{.*}}, ptr {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr {{.*}}, ptr {{.*}}, ptr @"$s18package_resilience33PublicClassWithFrozenPbPropertiesCMn")
// CHECK-NEXT:   ret ptr [[RESULT]]

// PkgClassWithFrozenPublicProperties metadata accessor
//3828
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s18package_resilience34PkgClassWithFrozenPublicPropertiesCMa"
// CHECK:      [[A:%.*]] = insertvalue %swift.metadata_response undef, ptr {{.*}}, 0
// CHECK-NEXT: [[B:%.*]] = insertvalue %swift.metadata_response [[A]]
// CHECK-NEXT: ret %swift.metadata_response [[B]]

// PkgClassWithFrozenPublicProperties method lookup
//3836
// CHECK: define{{.*}} swiftcc ptr @"$s18package_resilience34PkgClassWithFrozenPublicPropertiesCMu"(ptr {{.*}}, ptr {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT:   [[RESULT:%.*]] = call ptr @swift_lookUpClassMethod(ptr {{.*}}, ptr {{.*}}, ptr @"$s18package_resilience34PkgClassWithFrozenPublicPropertiesCMn")
// CHECK-NEXT:   ret ptr [[RESULT]]


