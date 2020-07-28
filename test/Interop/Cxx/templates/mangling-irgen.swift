// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-unknown-windows-msvc -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc x86_64-apple-macosx10.9 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc arm64-apple-ios11.2.0 -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
// R UN: %target-swift-frontend -Xcc -target -Xcc i386-apple-ios7.0-simulator -Xcc -fno-PIC -emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import Mangling

// MagicWrapper
// MagicWrapper<MagicNumber>
// __CxxTemplateInst12MagicWrapperI11MagicNumberE
public func read(_ i: inout WrappedMagicInt) {}

public func read(_ i: inout WrappedMagicBool) {}

// public struct Foo<T> {}
// public struct Bar {}

// public typealias FooA=Foo<Int>
// public typealias FooB=Foo<Bool>
// public typealias FooC=Foo<Bar>

// public func asdf(a: FooA) {}
// public func asdf(a: FooB) {}
// public func asdf(a: FooC) {}

// CHECK: asdasdaf
