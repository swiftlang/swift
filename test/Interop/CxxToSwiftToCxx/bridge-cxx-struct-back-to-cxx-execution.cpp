// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxx -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-public-decls

// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -g
// RUN: %target-interop-build-swift %t/use-cxx-types.swift -o %t/swift-cxx-execution -Xlinker %t/swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -g

// RUN: %target-codesign %t/swift-cxx-execution
// RUN: %target-run %t/swift-cxx-execution | %FileCheck %s

// REQUIRES: executable_test

//--- header.h

extern "C" void puts(const char *);

struct Trivial {
    int x, y;

    inline Trivial(int x, int y) : x(x), y(y) {}
};

template<class T>
struct NonTrivialTemplate {
    T x;

    inline NonTrivialTemplate(T x) : x(x) {
        puts("create NonTrivialTemplate");
    }
    inline NonTrivialTemplate(const NonTrivialTemplate<T> &other) : x(other.x) {
        puts("copy NonTrivialTemplate");
    }
    inline NonTrivialTemplate(NonTrivialTemplate<T> &&other) : x(static_cast<T &&>(other.x)) {
        puts("move NonTrivialTemplate");
    }
    inline ~NonTrivialTemplate() {
        puts("~NonTrivialTemplate");
    }
};

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func retNonTrivial(y: CInt) -> NonTrivialTemplate<Trivial> {
    return NonTrivialTemplate<Trivial>(Trivial(42, y))
}

public func takeNonTrivial(_ x: NonTrivialTemplate<Trivial>) {
    print(x)
}

public func passThroughNonTrivial(_ x: NonTrivialTemplate<Trivial>) -> NonTrivialTemplate<Trivial>{
    return x
}

public func inoutNonTrivial(_ x: inout NonTrivialTemplate<Trivial>) {
    x.x.y *= 2
}

public func retTrivial(_ x: CInt) -> Trivial {
    return Trivial(x, -x)
}

public func takeTrivial(_ x: Trivial) {
    print(x)
}

public func passThroughTrivial(_ x: Trivial) -> Trivial {
    return x
}

public func inoutTrivial(_ x: inout Trivial) {
    x.x = x.y + x.x - 11
}

public func takeGeneric<T>(_ x: T) {
    print("GENERIC", x)
}

public func retPassThroughGeneric<T>(_ x: T) -> T {
    return x
}

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <assert.h>

int main() {
  {
    auto x = UseCxx::retTrivial(423421);
    assert(x.x == 423421);
    assert(x.y == -423421);
    UseCxx::takeTrivial(UseCxx::passThroughTrivial(x));
    assert(x.x == 423421);
    assert(x.y == -423421);
    UseCxx::inoutTrivial(x);
    assert(x.x == -11);
    assert(x.y == -423421);
    UseCxx::takeTrivial(x);
    UseCxx::takeGeneric(x);
    auto xPrime = UseCxx::retPassThroughGeneric(x);
    assert(xPrime.x == -11);
    assert(xPrime.y == -423421);
    UseCxx::takeTrivial(xPrime);
  }
// CHECK: Trivial(x: 423421, y: -423421)
// CHECK-NEXT: Trivial(x: -11, y: -423421)
// CHECK-NEXT: GENERIC Trivial(x: -11, y: -423421)
// CHECK-NEXT: Trivial(x: -11, y: -423421)
  {
    auto x = UseCxx::retNonTrivial(-942);
    assert(x.x.y == -942);
    assert(x.x.x == 42);
    UseCxx::takeNonTrivial(UseCxx::passThroughNonTrivial(x));
    puts("done non trivial");
    UseCxx::inoutNonTrivial(x);
    assert(x.x.y == -1884);
    assert(x.x.x == 42);
    UseCxx::takeGeneric(x);
    {
      auto xPrime = UseCxx::retPassThroughGeneric(x);
      assert(xPrime.x.y == -1884);
      assert(xPrime.x.x == 42);
      UseCxx::takeNonTrivial(xPrime);
    }
    puts("secondon non trivial");
  }
  puts("EndOfTest");
// CHECK-NEXT: create NonTrivialTemplate
// CHECK-NEXT: move NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: copy NonTrivialTemplate
// CHECK-NEXT: move NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: copy NonTrivialTemplate
// CHECK-NEXT: __CxxTemplateInst18NonTrivialTemplateI7TrivialE(x: __C.Trivial(x: 42, y: -942))
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: done non trivial
// CHECK-NEXT: copy NonTrivialTemplate
// CHECK-NEXT: GENERIC __CxxTemplateInst18NonTrivialTemplateI7TrivialE(x: __C.Trivial(x: 42, y: -1884))
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: copy NonTrivialTemplate
// CHECK-NEXT: move NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: copy NonTrivialTemplate
// CHECK-NEXT: __CxxTemplateInst18NonTrivialTemplateI7TrivialE(x: __C.Trivial(x: 42, y: -1884))
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: secondon non trivial
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: EndOfTest
  return 0;
}
