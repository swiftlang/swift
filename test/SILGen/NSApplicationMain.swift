// RUN: %target-swift-frontend -emit-silgen -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s
// RUN: %target-swift-frontend -emit-ir -parse-as-library -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s -check-prefix=IR

// REQUIRES: OS=macosx

import Foundation
import AppKit

@NSApplicationMain
class MyDelegate: NSApplicationDelegate {}

// CHECK-LABEL: sil @main
// CHECK:         function_ref @NSApplicationMain
// IR-LABEL: define i32 @main
// IR:            call i32 @NSApplicationMain

// Ensure that we coexist with normal references to the functions we
// implicitly reference in the synthesized main.
func bar() {
  NSApplicationMain(Process.argc, Process.unsafeArgv)
}
