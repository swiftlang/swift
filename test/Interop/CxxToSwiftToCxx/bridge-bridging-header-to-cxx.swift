// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxx -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -import-objc-header %t/header.h

// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -g
// RUN: %target-interop-build-swift %t/use-cxx-types.swift -o %t/swift-cxx-execution -Xlinker %t/swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -g -import-objc-header %t/header.h

//--- header.h
struct CxxTy {
    int field;
};

//--- use-cxx-types.swift
public func foo() -> CxxTy {
    CxxTy()
}

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"

int main() {
    auto obj = UseCxx::foo();
}
