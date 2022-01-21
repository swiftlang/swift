// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Foo.swiftmodule -module-name Foo -enable-library-evolution %s -DFoo -I %S/Inputs/imports-clang-modules/ -serialized-path-obfuscate %S=%S_obfuscated

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Bar.swiftinterface -module-name Bar -enable-library-evolution %s -DBar -I %t -serialized-path-obfuscate %S=%S_obfuscated

// RUN: %target-swift-frontend -typecheck %s -I %t -serialized-path-obfuscate %S=%S_obfuscated -DFooBar

#if Foo

import A
public func fooFunc() {}

#endif

#if Bar

import Foo

func barFunc() {
	fooFunc()
}

#endif

#if FooBar

import Bar
import A

#endif
