// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/I)

// RUN: %target-swift-frontend -emit-module -module-name Foo %s -DFoo -emit-module-path %t/I/Foo.swiftmodule
// RUN: %target-swift-emit-module-interface(%t/Bar.swiftinterface) %s -module-name Bar -I %t/I -emit-private-module-interface-path %t/Bar.private.swiftinterface -skip-import-in-public-interface Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Bar.swiftinterface) -module-name Bar
// RUN: %target-swift-typecheck-module-from-interface(%t/Bar.private.swiftinterface) -module-name Bar -I %t/I

// RUN: %FileCheck %s --check-prefix=PUBLIC-INTERFACE < %t/Bar.swiftinterface
// RUN: %FileCheck %s --check-prefix=PRIVATE-INTERFACE < %t/Bar.private.swiftinterface

#if Foo

public func fooFunc() {}

#else

import Foo

public func barFunc() {}

#endif

// PUBLIC-INTERFACE-NOT: import Foo
// PRIVATE-INTERFACE: import Foo
