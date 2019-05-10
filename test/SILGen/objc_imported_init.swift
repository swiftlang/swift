// RUN: %target-swift-emit-silgen -verify -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s

import Foundation

// Ensure we emit allocating constructor thunks for ObjC initializers that
// were inherited.
// CHECK-LABEL: sil shared [serializable] [ossa] @$sSo3FooCABycfC : $@convention(method) (@thick Foo.Type) -> @owned Foo {
func foo() {
  _ = Foo()
}

