// Compile the imported module to a .swiftinterface and ensure the convenience
// init delegates through the subclasses correctly.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution -swift-version 5
// RUN: rm %t/Module.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t -L %t

// Make sure the same error is emitted when importing a .swiftmodule

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -module-name Module -enable-library-evolution -swift-version 5
// RUN: %target-typecheck-verify-swift -I %t -L %t

import Module

final class StorageBase: Base {
  var x: Int

  init(suppressesInheritance x: Int) { // expected-note {{declared here}}
    self.x = x
  }
}

_ = StorageBase() // expected-error {{suppressesInheritance}}

extension Base {
  public convenience init(conveniently: ()) {
    self.init()
  }
}

final class ConvenientClientBase: Base {
  var x: Int
}


final class ConvenientDefaultStorageBase: Base {
  // The addition of storage suppresses convenience initializer inheritance
  // The default does *not* suppress designated initializer inheritance
  var x: Int = 42
}

final class ConvenientDesignatedClientBase: Base {
  var x: Int

  init(designatedly: ()) {
    self.x = 42
  }
}

final class ConvenientDesignatedEmptyClientBase: Base {
  init(designatedly: ()) {
    super.init(arg: 42)
  }
}

_ = ConvenientClientBase(conveniently: ()) // expected-error {{cannot be constructed}}
_ = ConvenientDefaultStorageBase(conveniently: ()) // expected-error {{expected 'arg:'}} expected-error {{expected argument type 'Int'}}
_ = ConvenientDesignatedClientBase(conveniently: ()) // expected-error {{expected 'designatedly:}}
_ = ConvenientDesignatedEmptyClientBase(arg: 42) // expected-error {{argument passed to call that takes no arguments}}
