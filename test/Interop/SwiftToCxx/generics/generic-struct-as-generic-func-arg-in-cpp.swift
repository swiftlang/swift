// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/generics.swift -module-name Generics -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Generics-Swift.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -Werror=unguarded-availability -c %t/generics.cpp -I %t -o %t/generics.o
// RUN: %target-build-swift %t/generics.swift -o %t/generics -Xlinker %t/generics.o -module-name Generics -Xfrontend -entry-point-function-name -Xfrontend swiftMain

//--- generics.swift

public struct MyClass<T> {
    public var a: T
    public init(_ p: T) { self.a = p }
}

public func genericFunc<T>(_ p: T) -> T {
    return p
}

@available(macOS 99, *)
public struct FutureStruct {
    public var a: Int
    public init() { self.a = 0 }
}

public func genericSize<T>(_ p: T) -> Int {
    return MemoryLayout<T>.size
}

//--- generics.cpp

#include "Generics-Swift.h"
using namespace Generics;

int main() {
  auto c = MyClass<int>::init(10);
  auto result = genericFunc<MyClass<int>>(c);
#if defined(__APPLE__)
  // The generic bridge must not warn about the type's availability.
  if (__builtin_available(macOS 99, *)) {
    auto future = FutureStruct::init();
    (void)genericSize(future);
  }
#endif
}
