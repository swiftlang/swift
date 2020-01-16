// Compile the imported module to a .swiftinterface and ensure the convenience
// init delegates through the subclasses correctly.

// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution
// RUN: rm %t/Module.swiftmodule
// RUN: %target-build-swift %s -I %t -L %t -lModule -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(Module)
// RUN: %target-run %t/main %t/%target-library-name(Module) | %FileCheck %s

// Make sure the same error is emitted when importing a .swiftmodule

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Module)) %S/inherits-superclass-initializers.swift -emit-module-path %t/Module.swiftmodule -module-name Module -enable-library-evolution
// RUN: %target-build-swift %s -I %t -L %t -lModule -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(Module)
// RUN: %target-run %t/main %t/%target-library-name(Module) | %FileCheck %s

import Module

_ = Base()
// CHECK: secret init from Base

_ = Sub()
// CHECK: secret init from Sub
// CHECK: secret init from Base

_ = SubSub()
// CHECK: secret init from SubSub
// CHECK: secret init from Sub
// CHECK: secret init from Base

test()
// CHECK: secret init from Sub
// CHECK: secret init from Base

// CHECK: secret init from SubSub
// CHECK: secret init from Sub
// CHECK: secret init from Base

// CHECK-NOT: public init
