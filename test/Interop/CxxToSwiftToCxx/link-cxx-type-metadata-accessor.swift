// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-cxx-types.swift -module-name UseCxx -typecheck -verify -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public

// Use the 'DEBUG' flug to force llvm used
// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -DDEBUG
// RUN: %target-interop-build-swift %t/use-cxx-types.swift -o %t/swift-cxx-execution -Xlinker %t/swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t

//--- header.h

struct CxxStruct {
    inline CxxStruct(int x) : x(x) {}
    inline CxxStruct(const CxxStruct &other) : x(other.x) {}
    inline ~CxxStruct() {}

    int x;
};

struct CxxStruct2 {
    inline CxxStruct2(int x) : x(x) {}
    inline CxxStruct2(const CxxStruct &other) : x(other.x) {}
    inline ~CxxStruct2() {}

    int x;
};

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func retCxxStruct() -> CxxStruct {
    return CxxStruct(2)
}

#if !os(Windows)
public func retCxxStruct2() -> CxxStruct2? {
    return CxxStruct2(2)
}
#endif

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <assert.h>

int main() {
  auto x = UseCxx::retCxxStruct();
#ifndef _WIN32
  auto y = UseCxx::retCxxStruct2();
#endif
  return 0;
}
