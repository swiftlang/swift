// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/print-string.swift -module-name Stringer -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Stringer.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %t/string-conversions.cpp -I %t -o %t/swift-stdlib-execution.o -DDEBUG=1
// RUN: %target-build-swift %t/print-string.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name Stringer -Xfrontend -entry-point-function-name -Xfrontend swiftMain %target-cxx-lib
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// Ensure that this works in optimized mode:

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %t/string-conversions.cpp -I %t -o %t/swift-stdlib-execution-opt.o -O
// RUN: %target-build-swift %t/print-string.swift -o %t/swift-stdlib-execution-opt -Xlinker %t/swift-stdlib-execution-opt.o -module-name Stringer -Xfrontend -entry-point-function-name -Xfrontend swiftMain %target-cxx-lib -O
// RUN: %target-codesign %t/swift-stdlib-execution-opt
// RUN: %target-run %t/swift-stdlib-execution-opt | %FileCheck %s

// REQUIRES: executable_test

//--- print-string.swift

@_expose(Cxx)
public func printString(_ s: String) {
    print("'''\(s)'''")
}

//--- string-conversions.cpp

#include <cassert>
#include "Stringer.h"

int main() {
  using namespace swift;
  using namespace Stringer;

  {
    auto s = String("Hello world");
    assert(s.getCount() == 11);
    printString(s);
    s.append(String("!test"));
    printString(s);
    printString(s.lowercased());
    printString(s.uppercased());
  }
// CHECK: '''Hello world'''
// CHECK: '''Hello world!test'''
// CHECK: '''hello world!test'''
// CHECK: '''HELLO WORLD!TEST'''
  return 0;
}
