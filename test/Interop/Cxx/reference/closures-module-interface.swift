// RUN: %target-swift-ide-test -print-module -module-to-print=Closures -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func invokeWith42ConstRef(_ fn: ((CInt) -> Void)!)
// CHECK: func invokeWith42Ref(_ fn: ((UnsafeMutablePointer<CInt>) -> Void)!)
