// RUN: %target-swift-remoteast-test-with-sdk -I %S/../ClangImporter/Inputs/custom-modules -I %S/../Inputs/custom-modules %s | %FileCheck %s

// REQUIRES: swift-remoteast-test
// REQUIRES: objc_interop

import Foundation
import CoreCooling
import ErrorEnums

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

printType(CCRefrigerator.self)
// CHECK: found type: CCRefrigerator

printType(MyError.self)
// CHECK: found type: MyError{{$}}

printType(MyError.Code.self)
// CHECK: found type: MyError.Code{{$}}

printType(RenamedError.self)
// CHECK: found type: RenamedError{{$}}

printType(RenamedError.Code.self)
// CHECK: found type: RenamedError.Code{{$}}

printType(Wrapper.MemberEnum.self)
// CHECK: found type: Wrapper.MemberEnum{{$}}

printType(WrapperByAttribute.self)
// CHECK: found type: WrapperByAttribute{{$}}
