// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/binary)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/module-cache)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/binary/Foo.swiftmodule -parse-as-library %s -enable-library-evolution -allowable-client Bar -allowable-client FooBar -module-name Foo -DFOO -emit-module-interface-path %t/textual/Foo.swiftinterface

// RUN: %target-swift-frontend -typecheck %s -I %t/binary -module-name Bar
// RUN: %target-swift-frontend -typecheck %s -I %t/binary -module-name FooBar
// RUN: %target-typecheck-verify-swift -I %t/binary -module-name Blocked
// RUN: %target-typecheck-verify-swift -I %t/textual -module-name Blocked -module-cache-path %t/module-cache

#if FOO

public func foo() {}

#else

import Foo // expected-error {{module 'Foo' doesn't allow importation from module 'Blocked'}}

func bar() { foo() }

#endif
