// UNSUPPORTED: -windows-msvc

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/Mods/Foo.swiftmodule
// RUN: mkdir -p %t/Mods/Bar.swiftmodule

// RUN: %target-swift-frontend %s -DFoo -emit-module -o %t/Mods/Foo.swiftmodule/armv7s-apple-ios.swiftmodule -emit-module-interface-path %t/Mods/Foo.swiftmodule/armv7s-apple-ios.swiftinterface -target armv7s-apple-ios10 -module-name Foo -enable-library-evolution -parse-stdlib
// RUN: %target-swift-frontend %s -DFoo -emit-module -o %t/Mods/Foo.swiftmodule/arm.swiftmodule -emit-module-interface-path %t/Mods/Foo.swiftmodule/arm.swiftinterface -target arm-apple-ios10 -module-name Foo -enable-library-evolution -parse-stdlib
// RUN: %target-swift-frontend %s -DBar -typecheck -emit-module-interface-path %t/Mods/Bar.swiftmodule/arm.swiftinterface -I %t/Mods -target arm-apple-ios10 -module-name Bar -enable-library-evolution -parse-stdlib
// RUN: %target-swift-frontend %s -DFooBar -typecheck -emit-loaded-module-trace-path - -I %t/Mods -target armv7s-apple-ios10 -parse-stdlib

#if Foo
#endif

#if Bar
import Foo
#endif

#if FooBar
import Foo
import Bar
#endif
