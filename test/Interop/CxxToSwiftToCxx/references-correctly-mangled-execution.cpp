// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-cxx-types.swift -typecheck -module-name UseCxx -emit-clang-header-path %t/UseCxx.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public

// RUN: %target-interop-build-clangxx -std=c++20 -c %t/use-swift-cxx-types.cpp -I %t -o %t/swift-cxx-execution.o -g
// RUN: %target-interop-build-swift %t/use-cxx-types.swift -o %t/swift-cxx-execution -Xlinker %t/swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t -g

// RUN: %target-codesign %t/swift-cxx-execution
// RUN: %target-run %t/swift-cxx-execution

// REQUIRES: executable_test

//--- header.h
#include <functional>
namespace my_cpp {
struct First {
  double value;
};
struct Second {
  bool value;
};

using First_cb = std::function<void(const First &)>;
using Second_cb = std::function<void(const Second &)>;
} // namespace my_cpp

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxStdlib
import CxxTest

public func
hello(first : my_cpp.First_cb /* std::function */,
      second : my_cpp.Second_cb /* std::function */) {
  first.callAsFunction(my_cpp.First(value : 3.14))
      second.callAsFunction(my_cpp.Second(value : true))
}

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"
#include <assert.h>

int main() {
  UseCxx::hello([](const my_cpp::First &) {}, [](const my_cpp::Second &) {});
}