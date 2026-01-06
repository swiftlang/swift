// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Generics-Swift.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/generics.cpp -I %t -o %t/generics.o
// RUN: %target-build-swift %t/generics.swift -o %t/generics -Xlinker %t/generics.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

//--- generics.swift

public struct MyClass<T> {
    public var a: T
    public init(_ p: T) { self.a = p }
}

public func genericFunc<T>(_ p: T) -> T {
    return p
}

//--- generics.cpp

#include "Generics-Swift.h"
using namespace Generics;

int main() {
  auto c = MyClass<int>::init(10);
  auto result = genericFunc<MyClass<int>>(c);
}
