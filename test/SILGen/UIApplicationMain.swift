// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %build-silgen-test-overlays-ios

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-emit-silgen(mock-sdk: -Xcc -DSILGEN_TEST_UIAPPLICATIONMAIN_NULLABILITY -sdk %S/Inputs -I %t) -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -Xcc -DSILGEN_TEST_UIAPPLICATIONMAIN_NULLABILITY -sdk %S/Inputs -I %t) -parse-as-library %s | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s -D REFERENCE | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s -D REFERENCE | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library -primary-file %s %S/Inputs/UIApplicationMain-helper.swift -module-name test | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library -primary-file %s %S/Inputs/UIApplicationMain-helper.swift -module-name test | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s %S/Inputs/UIApplicationMain-helper.swift -module-name test | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %s %S/Inputs/UIApplicationMain-helper.swift -module-name test | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %S/Inputs/UIApplicationMain-helper.swift %s -module-name test | %FileCheck %s
// RUN: %target-swift-emit-ir(mock-sdk: -sdk %S/Inputs -I %t) -parse-as-library %S/Inputs/UIApplicationMain-helper.swift %s -module-name test | %FileCheck %s -check-prefix=IR

// REQUIRES: OS=ios
// REQUIRES: objc_interop

import Foundation
import UIKit

@UIApplicationMain
class MyDelegate : UIApplicationDelegate {}

// CHECK-LABEL: sil @main
// CHECK:         function_ref @UIApplicationMain
// IR-LABEL: define{{( protected)?}} i32 @main
// IR:            call i32 @UIApplicationMain

// Ensure that we coexist with normal references to the functions we
// implicitly reference in the synthesized main.
#if REFERENCE
func foo(x: AnyObject.Type) -> String {
  return NSStringFromClass(x)
}

func bar() {
  UIApplicationMain(0, nil, nil, nil)
}
#endif
