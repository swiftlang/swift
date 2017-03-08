// RUN: %target-swift-frontend -emit-silgen -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-frontend -emit-silgen -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -D REFERENCE | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -D REFERENCE | %FileCheck %s -check-prefix=IR

// REQUIRES: OS=macosx

import Foundation
import AppKit

@NSApplicationMain
class MyDelegate: NSApplicationDelegate {}

// CHECK-LABEL: sil @main
// CHECK:         function_ref @NSApplicationMain
// IR-LABEL: define{{( protected)?}} i32 @main
// IR:            call swiftcc i32 @NSApplicationMain

#if REFERENCE
// Ensure that we coexist with normal references to the functions we
// implicitly reference in the synthesized main.
func bar() {
  NSApplicationMain(CommandLine.argc, CommandLine.unsafeArgv)
}
#endif
