// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxx -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-public-decls

// RUN: %target-interop-build-clangxx -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -g
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
    inline NonTrivialTemplate(const NonTrivialTemplate<T> &) = default;
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

public func retTrivial(_ x: CInt) -> Trivial {
    return Trivial(x, -x)
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
  }
  {
    auto x = UseCxx::retNonTrivial(-942);
    assert(x.x.y == -942);
    assert(x.x.x == 42);
  }
// CHECK: create NonTrivialTemplate
// CHECK-NEXT: move NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
// CHECK-NEXT: ~NonTrivialTemplate
  return 0;
}
