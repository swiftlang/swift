// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen -parse-as-library %s | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-ir -parse-as-library %s | %FileCheck %s -check-prefix=IR

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen -parse-as-library %s -D REFERENCE | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-ir -parse-as-library -enable-source-import %s -D REFERENCE | %FileCheck %s -check-prefix=IR

// REQUIRES: OS=macosx

import Foundation
import AppKit

@NSApplicationMain
class MyDelegate: NSApplicationDelegate {}

// CHECK-LABEL: sil @main
// CHECK:         function_ref @NSApplicationMain
// IR-LABEL: define{{( protected)?}} i32 @main
// IR:            call i32 @NSApplicationMain

#if REFERENCE
// Ensure that we coexist with normal references to the functions we
// implicitly reference in the synthesized main.
func bar() {
  NSApplicationMain(CommandLine.argc, CommandLine.unsafeArgv)
}
#endif
