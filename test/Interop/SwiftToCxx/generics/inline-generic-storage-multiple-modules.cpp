// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/first.swift -module-name First \
// RUN:   -cxx-interoperability-mode=default -typecheck -emit-clang-header-path %t/first.h
// RUN: %target-swift-frontend %t/second.swift -module-name Second \
// RUN:   -cxx-interoperability-mode=default -enable-library-evolution \
// RUN:   -typecheck -emit-clang-header-path %t/second.h
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/producer.cpp -I %t -o %t/producer.o
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/main.cpp -I %t -o %t/main.o
// RUN: %target-interop-build-swift %t/first.swift -module-name First \
// RUN:   -parse-as-library -emit-object -o %t/first.o
// RUN: %target-interop-build-swift %t/second.swift -module-name Second \
// RUN:   -enable-library-evolution -o %t/main \
// RUN:   -Xlinker %t/first.o -Xlinker %t/producer.o -Xlinker %t/main.o \
// RUN:   -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test

//--- first.swift
public func makeArray() -> [Int32] { [11, 22] }

//--- second.swift
public func passArray(_ value: [Int32]) -> [Int32] { value }
public func appendArray(_ value: inout [Int32]) { value.append(33) }
public func consumeArray(_ value: consuming [Int32]) -> Int32 {
  value.reduce(0, +)
}

//--- producer.cpp
#include "first.h"
#include "second.h"

static_assert(!swift::_impl::isOpaqueLayout<swift::Array<int32_t>>);

swift::Array<int32_t> makeArrayInOtherTranslationUnit() {
  return Second::passArray(First::makeArray());
}

//--- main.cpp
// Include the generated headers in the opposite order in this translation unit.
#include "second.h"
#include "first.h"
#include <cassert>

static_assert(!swift::_impl::isOpaqueLayout<swift::Array<int32_t>>);

swift::Array<int32_t> makeArrayInOtherTranslationUnit();

int main() {
  auto array = makeArrayInOtherTranslationUnit();
  assert(array.getCount() == 2 && array[0] == 11 && array[1] == 22);
  auto copy = array;
  Second::appendArray(copy);
  assert(copy.getCount() == 3 && copy[2] == 33);
  assert(array.getCount() == 2);
  assert(Second::consumeArray(copy) == 66);
  assert(copy.getCount() == 3);
}
