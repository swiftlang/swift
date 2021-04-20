// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import Extensions

extension Outer.Space.Foo {
  func foo() {}
}

Outer.Space.Foo().foo()

// CHECK: call swiftcc void @"$sSo5OuterO5SpaceO10ExtensionsE3FooV4mainE3fooyyF"

// CHECK: define hidden swiftcc void @"$sSo5OuterO5SpaceO10ExtensionsE3FooV4mainE3fooyyF"
