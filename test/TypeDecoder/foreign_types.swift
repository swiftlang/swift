// RUN: %empty-directory(%t)

// RUN: %target-build-swift -I %S/../ClangImporter/Inputs/custom-modules -I %S/../Inputs/custom-modules -emit-executable -emit-module %s -g -o %t/foreign_types

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/foreign_types -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/foreign_types -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

// REQUIRES: objc_interop

import Foundation
import CoreCooling
import ErrorEnums

extension CCRefrigerator {
    struct InternalNestedType {}
    fileprivate struct PrivateNestedType {}
}

/*
do {
    let x1 = CCRefrigeratorCreate(kCCPowerStandard)
    let x2 = MyError.good
    let x3 = MyError.Code.good
    let x4 = RenamedError.good
    let x5 = RenamedError.Code.good
    let x5b = Wrapper.MemberError.A
    let x5c = Wrapper.MemberError.Code.A
    let x6 = Wrapper.MemberEnum.A
    let x7 = WrapperByAttribute(0)
    let x8 = IceCube(width: 0, height: 0, depth: 0)
    let x9 = BlockOfIce(width: 0, height: 0, depth: 0)
}

do {
    let x1 = CCRefrigerator.self
    let x2 = MyError.self
    let x3 = MyError.Code.self
    let x4 = RenamedError.self
    let x5 = RenamedError.Code.self
    let x5b = Wrapper.MemberError.self
    let x5c = Wrapper.MemberError.Code.self
    let x6 = Wrapper.MemberEnum.self
    let x7 = WrapperByAttribute.self
    let x8 = IceCube.self
    let x9 = BlockOfIce.self
}
*/

// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefaD
// DEMANGLE-TYPE: $sSo7MyErrorVD
// DEMANGLE-TYPE: $sSo7MyErrorLeVD
// DEMANGLE-TYPE: $sSo14MyRenamedErrorVD
// DEMANGLE-TYPE: $sSo14MyRenamedErrorLeVD
// DEMANGLE-TYPE: $sSo13MyMemberErrorVD
// DEMANGLE-TYPE: $sSo13MyMemberErrorLeVD
// DEMANGLE-TYPE: $sSo12MyMemberEnumVD
// DEMANGLE-TYPE: $sSo18WrapperByAttributeaD
// DEMANGLE-TYPE: $sSo7IceCubeVD
// DEMANGLE-TYPE: $sSo10BlockOfIceaD
// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefa13foreign_typesE18InternalNestedTypeVD
// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefa13foreign_typesE17PrivateNestedType33_5415CB6AE6FCD935BF2278A4C9A5F9C3LLVD

// CHECK-TYPE: CCRefrigerator
// CHECK-TYPE: MyError.Code
// CHECK-TYPE: MyError
// CHECK-TYPE: RenamedError.Code
// CHECK-TYPE: RenamedError
// CHECK-TYPE: Wrapper.MemberError.Code
// CHECK-TYPE: Wrapper.MemberError
// CHECK-TYPE: Wrapper.MemberEnum
// CHECK-TYPE: WrapperByAttribute
// CHECK-TYPE: IceCube
// CHECK-TYPE: BlockOfIce
// CHECK-TYPE: CCRefrigerator.InternalNestedType
// CHECK-TYPE: CCRefrigerator.PrivateNestedType

// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefamD
// DEMANGLE-TYPE: $sSo7MyErrorVmD
// DEMANGLE-TYPE: $sSC7MyErrorLeVmD
// DEMANGLE-TYPE: $sSo14MyRenamedErrorVmD
// DEMANGLE-TYPE: $sSC14MyRenamedErrorLeVmD
// DEMANGLE-TYPE: $sSo13MyMemberErrorVmD
// DEMANGLE-TYPE: $sSC13MyMemberErrorLeVmD
// DEMANGLE-TYPE: $sSo12MyMemberEnumVmD
// DEMANGLE-TYPE: $sSo18WrapperByAttributeamD
// DEMANGLE-TYPE: $sSo7IceCubeVmD
// DEMANGLE-TYPE: $sSo10BlockOfIceamD
// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefa13foreign_typesE18InternalNestedTypeVmD
// DEMANGLE-TYPE: $sSo17CCRefrigeratorRefa13foreign_typesE17PrivateNestedType33_5415CB6AE6FCD935BF2278A4C9A5F9C3LLVmD

// CHECK-TYPE: CCRefrigerator.Type
// CHECK-TYPE: MyError.Code.Type
// CHECK-TYPE: MyError.Type
// CHECK-TYPE: RenamedError.Code.Type
// CHECK-TYPE: RenamedError.Type
// CHECK-TYPE: Wrapper.MemberError.Code.Type
// CHECK-TYPE: Wrapper.MemberError.Type
// CHECK-TYPE: Wrapper.MemberEnum.Type
// CHECK-TYPE: WrapperByAttribute.Type
// CHECK-TYPE: IceCube.Type
// CHECK-TYPE: BlockOfIce.Type
// CHECK-TYPE: CCRefrigerator.InternalNestedType.Type
// CHECK-TYPE: CCRefrigerator.PrivateNestedType.Type

// DEMANGLE-DECL: $sSo17CCRefrigeratorRefa
// DEMANGLE-DECL: $sSo7MyErrorV
// DEMANGLE-DECL: $sSo7MyErrorLeV
// DEMANGLE-DECL: $sSo14MyRenamedErrorV
// DEMANGLE-DECL: $sSo14MyRenamedErrorLeV
// DEMANGLE-DECL: $sSo13MyMemberErrorV
// DEMANGLE-DECL: $sSo13MyMemberErrorLeV
// DEMANGLE-DECL: $sSo12MyMemberEnumV
// DEMANGLE-DECL: $sSo18WrapperByAttributea
// DEMANGLE-DECL: $sSo7IceCubeV
// DEMANGLE-DECL: $sSo10BlockOfIcea
// DEMANGLE-DECL: $sSo17CCRefrigeratorRefa13foreign_typesE18InternalNestedTypeV
// DEMANGLE-DECL: $sSo17CCRefrigeratorRefa13foreign_typesE17PrivateNestedType33_5415CB6AE6FCD935BF2278A4C9A5F9C3LLV

// CHECK-DECL: CoreCooling.(file).CCRefrigerator
// CHECK-DECL: ErrorEnums.(file).MyError.Code
// CHECK-DECL: ErrorEnums.(file).MyError.Code
// CHECK-DECL: ErrorEnums.(file).RenamedError.Code
// CHECK-DECL: ErrorEnums.(file).RenamedError.Code
// CHECK-DECL: ErrorEnums.(file).Wrapper extension.MemberError.Code
// CHECK-DECL: ErrorEnums.(file).Wrapper extension.MemberError.Code
// CHECK-DECL: ErrorEnums.(file).Wrapper extension.MemberEnum
// CHECK-DECL: ErrorEnums.(file).WrapperByAttribute
// CHECK-DECL: CoreCooling.(file).IceCube
// CHECK-DECL: CoreCooling.(file).BlockOfIce
// CHECK-DECL: foreign_types.(file).CCRefrigerator extension.InternalNestedType
// CHECK-DECL: foreign_types.(file).CCRefrigerator extension.PrivateNestedType
