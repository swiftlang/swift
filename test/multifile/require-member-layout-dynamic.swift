// RUN: %target-swift-frontend -module-name test -emit-ir -primary-file %s %S/Inputs/require-member-layout-dynamic-other.swift -import-objc-header %S/Inputs/require-member-layout-dynamic-bridging.h -sdk %sdk -o %t.o

// REQUIRES: objc_interop

import Foundation

public class Foo: NSObject {
  func foo() {
    bar("hello")
  }
}
