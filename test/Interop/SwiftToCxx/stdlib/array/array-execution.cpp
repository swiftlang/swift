// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftinterface -enable-library-evolution -disable-objc-attr-requires-foundation-module -typecheck -module-name Swift -parse-stdlib -enable-experimental-cxx-interop -emit-clang-header-path %t/Swift.h  -experimental-skip-all-function-bodies

// RUN: %target-swift-frontend -typecheck %t/use-array.swift -typecheck -module-name UseArray -enable-experimental-cxx-interop -emit-clang-header-path %t/UseArray.h

// RUN: %target-interop-build-clangxx -fno-exceptions -std=gnu++20 -c %t/array-execution.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/use-array.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name UseArray -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// REQUIRES: executable_test

//--- use-array.swift
@_expose(Cxx)
public func createArray(_ val: CInt) -> [CInt] {
    return [val, val]
}

@_expose(Cxx)
public func passthroughArray(_ val: [CInt]) -> Array<CInt> {
    return val
}

@_expose(Cxx)
public func printArray(_ val: Array<CInt>) {
    print(val)
}

//--- array-execution.cpp

#include <cassert>
#include "Swift.h"
#include "UseArray.h"

int main() {
  using namespace Swift;

  {
    Array<int> val = UseArray::createArray(2);
    UseArray::printArray(UseArray::passthroughArray(val));
  }
// CHECK: [2, 2]
  {
    auto val = Array<int>::init();
    val.append(-11);
    UseArray::printArray(val);
    assert(val.getCount() == 1);
    assert(val.getCapacity() >= 1);
    auto zeroInt = val[0];
    assert(zeroInt == -11);
    auto firstInt = val.remove(0);
    assert(firstInt == -11);
    assert(val.getCount() == 0);
    UseArray::printArray(val);
  }
// CHECK-NEXT: [-11]
// CHECK-NEXT: []
  return 0;
}
