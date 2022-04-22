// RUN: %target-swift-ide-test -print-module -module-to-print=ProtocolNamingConflict -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop -enable-objc-interop | %FileCheck -check-prefix=CHECK-IDE-TEST %s
// RUN: %swift-frontend -c -enable-experimental-cxx-interop -enable-objc-interop -I %S/Inputs %s -emit-sil -o - | %FileCheck %s

// REQUIRES: objc_interop

import ProtocolNamingConflict

// CHECK: class Thing : FooProtocol
// CHECK-IDE-TEST: protocol FooProtocol
// CHECK-IDE-TEST: class Foo : FooProtocol
class Thing: FooProtocol {
}
