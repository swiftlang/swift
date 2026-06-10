// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Emit swiftinterfaces for the library and verify that they contain the expected contents.
// RUN: %target-swift-emit-module-interfaces(%t/Module.swiftinterface, %t/Module.private.swiftinterface) %t/Module.swift -module-name Module -package-name TestPackage
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.swiftinterface) -module-name Module
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.private.swiftinterface) -module-name Module
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-PUBLIC < %t/Module.swiftinterface
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-PRIVATE < %t/Module.private.swiftinterface

// Compile the client against the private .swiftinterface
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

// Compile the client against the public .swiftinterface.
// RUN: rm %t/Module.private.swiftinterface
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t -verify-additional-prefix public-

// Make sure the same error is emitted when importing a .swiftmodule
// RUN: rm %t/Module.swiftinterface
// RUN: %target-swift-frontend -emit-module -o %t/Module.swiftmodule %t/Module.swift -module-name Module -package-name TestPackage -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t

//--- Module.swift

// CHECK: @_hasMissingDesignatedInitializers open class HasInternalInit {
open class HasInternalInit {
  init() {}
  // CHECK: public init(_: Swift::Int)
  public init(_: Int) {}
  // CHECK: convenience public init(hi: ())
  public convenience init(hi: ()) { self.init() }
// CHECK: }
}

// CHECK: @_hasMissingDesignatedInitializers open class HasPackageInit {
open class HasPackageInit {
  package init() {}
  // CHECK: public init(_: Swift::Int)
  public init(_: Int) {}
  // CHECK: convenience public init(hi: ())
  public convenience init(hi: ()) { self.init() }
// CHECK: }
}

// CHECK-PUBLIC: @_hasMissingDesignatedInitializers open class HasSPIInit {
// CHECK-PRIVATE: {{^}}open class HasSPIInit {
open class HasSPIInit {
  // CHECK-PRIVATE-NEXT: @_spi(Private) public init()
  @_spi(Private) public init() {}
  // CHECK: public init(_: Swift::Int)
  public init(_: Int) {}
  // CHECK: convenience public init(hi: ())
  public convenience init(hi: ()) { self.init() }
// CHECK: }
}

//--- Client.swift

import Module

open class HasInternalInitDerived : HasInternalInit {
  var x: Int = 0
}

_ = HasInternalInitDerived(hi: ())
// expected-error@-1 {{cannot convert value of type '()' to expected argument type 'Int'}}
// expected-error@-2 {{extraneous argument label 'hi:' in call}}

open class HasPackageInitDerived : HasPackageInit {
  var x: Int = 0
}

_ = HasPackageInitDerived(hi: ())
// expected-error@-1 {{cannot convert value of type '()' to expected argument type 'Int'}}
// expected-error@-2 {{extraneous argument label 'hi:' in call}}

open class HasSPIInitDerived : HasSPIInit {
  var x: Int = 0
}

_ = HasSPIInitDerived(hi: ())
// expected-public-error@-1 {{cannot convert value of type '()' to expected argument type 'Int'}}
// expected-public-error@-2 {{extraneous argument label 'hi:' in call}}

