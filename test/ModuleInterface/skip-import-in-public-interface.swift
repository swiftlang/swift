// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/I)

// RUN: %target-swift-frontend -emit-module -module-name Foo %s -DFoo -emit-module-path %t/I/Foo.swiftmodule
// RUN: %target-swift-frontend -typecheck -module-name Bar %s -I %t/I -emit-module-interface-path %t/Bar.swiftinterface -emit-private-module-interface-path %t/Bar.private.swiftinterface -skip-import-in-public-interface Foo

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
