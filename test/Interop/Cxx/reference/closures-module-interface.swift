// RUN: %target-swift-ide-test -print-module -module-to-print=Closures -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func invokeWith42ConstRef(_ fn: ((Int32) -> Void)!)
// CHECK: func invokeWith42Ref(_ fn: ((UnsafeMutablePointer<Int32>) -> Void)!)
