// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-module-interface(%t/DefinesProtocol.swiftinterface) -I %t %t/DefinesProtocol.swift -module-name DefinesProtocol
// RUN: %target-swift-typecheck-module-from-interface(%t/DefinesProtocol.swiftinterface) -I %t -module-name DefinesProtocol

// RUN: %target-swift-emit-module-interface(%t/DefinesTypealias.swiftinterface) -I %t %t/DefinesTypealias.swift -module-name DefinesTypealias
// RUN: %target-swift-typecheck-module-from-interface(%t/DefinesTypealias.swiftinterface) -I %t -module-name DefinesTypealias

// RUN: %target-swift-emit-module-interface(%t/DefinesStruct.swiftinterface) -I %t %t/DefinesStruct.swift -module-name DefinesStruct
// RUN: %target-swift-typecheck-module-from-interface(%t/DefinesStruct.swiftinterface) -I %t -module-name DefinesStruct

// RUN: %target-swift-emit-module-interface(%t/DefinesExtension.swiftinterface) -I %t %t/DefinesExtension.swift -module-name DefinesExtension
// RUN: %target-swift-typecheck-module-from-interface(%t/DefinesExtension.swiftinterface) -I %t -module-name DefinesExtension

// RUN: %target-swift-emit-module-interface(%t/Client.swiftinterface) -verify -I %t %t/File2.swift %t/File1.swift -module-name Client
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) -I %t -module-name Client
// RUN: %FileCheck %t/File2.swift < %t/Client.swiftinterface

//--- DefinesProtocol.swift

public protocol P {}

//--- DefinesTypealias.swift

import DefinesProtocol

public typealias Q = P

//--- DefinesStruct.swift

public struct C {}

//--- DefinesExtension.swift

import DefinesStruct

extension C {
  public func foo() {}
}

//--- File1.swift

import DefinesExtension

//--- File2.swift

// CHECK:       import DefinesExtension
// CHECK-NEXT:  import DefinesProtocol
// CHECK-NEXT:  import DefinesStruct
// CHECK-NEXT:  import DefinesTypealias

import DefinesTypealias
import DefinesStruct

public func takesQ<T>(_ b: T) where T: Q { }
// expected-warning @-1 {{'Q' aliases 'DefinesProtocol.P' and cannot be used here because 'DefinesProtocol' was not imported by this file; this is an error in the Swift 6 language mode}}
// expected-note @-2 {{The missing import of module 'DefinesProtocol' will be added implicitly}}

@inlinable public func takesC(_ c: C) {
  c.foo()
  // expected-warning @-1 {{instance method 'foo()' cannot be used in an '@inlinable' function because 'DefinesExtension' was not imported by this file; this is an error in the Swift 6 language mode}}
  // expected-note @-2 {{The missing import of module 'DefinesExtension' will be added implicitly}}
}
