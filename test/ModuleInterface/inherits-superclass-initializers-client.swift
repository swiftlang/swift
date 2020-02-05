// Compile the imported module to a .swiftinterface and ensure the convenience
// init delegates through the subclasses correctly.

// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution -swift-version 5
// RUN: rm %t/Module.swiftmodule
// RUN: %target-build-swift %s -I %t -L %t -lModule -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(Module)
// RUN: %target-run %t/main %t/%target-library-name(Module) | %FileCheck %s

// Make sure the same error is emitted when importing a .swiftmodule

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -module-name Module -enable-library-evolution -swift-version 5
// RUN: %target-build-swift %s -I %t -L %t -lModule -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(Module)
// RUN: %target-run %t/main %t/%target-library-name(Module) | %FileCheck %s

import Module

// rdar://59171169 Even though Base has missing designated initializers, as long
// as this client doesn't add more storage that would require additional
// designated initializers to handle we should still inherit convenience
// intializers.
final class ClientSub: Base {}

public class ConvenientClientBase: Base {
  // does not add storage, does not override any designated inits
}

final class ConvenientDefaultStorageBase: Base {
  var x: Int = 42
}

final class ConvenientStaticClientBase: Base {
  static var x: Int = 42
}

final class ConvenientDesignatedEmptyClientBase: Base {
  // Suppresses designated init inheritance. Convenience initializer inheritance
  // is not suppressed because there are no stored properties.
  init(designatedly: ()) {
    super.init(arg: 42)
  }
}

extension Base {
  public convenience init(conveniently: ()) {
    self.init()
  }
}


_ = Base()
// CHECK: secret init from Base

_ = Sub()
// CHECK: secret init from Sub
// CHECK: secret init from Base

_ = SubSub()
// CHECK: secret init from SubSub
// CHECK: secret init from Sub
// CHECK: secret init from Base

_ = ClientSub()
// CHECK: secret init from Base

_ = ConvenientClientBase()
// CHECK: secret init from Base

_ = ConvenientClientBase(conveniently: ())
// CHECK: secret init from Base

let store = ConvenientDefaultStorageBase(arg: 42)
print(store.x)
// CHECK: public init from Base
// CHECK: 42

_ = ConvenientStaticClientBase(conveniently: ())
// CHECK: secret init from Base

_ = ConvenientDesignatedEmptyClientBase()
// CHECK: secret init from Base

_ = ConvenientDesignatedEmptyClientBase(conveniently: ())
// CHECK: secret init from Base

test()
// CHECK: secret init from Sub
// CHECK: secret init from Base

// CHECK: secret init from SubSub
// CHECK: secret init from Sub
// CHECK: secret init from Base

// CHECK-NOT: public init
