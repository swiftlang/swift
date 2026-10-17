// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %check-interop-cxx-header-in-clang(%test-resource-dir/swift/Swift.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-private-field -Wno-unused-function)

// Existing module headers keep their inline standard library bindings. They
// can be included alongside the public header in either order.
// RUN: %target-swift-frontend %t/Record.swift -module-name Record -typecheck -verify -cxx-interoperability-mode=default -emit-clang-header-path %t/Record.h
// RUN: %target-swift-frontend %t/Text.swift -module-name Text -typecheck -verify -cxx-interoperability-mode=default -emit-clang-header-path %t/Text.h
// RUN: %FileCheck %s --check-prefix=INLINE --implicit-check-not='include <swift/Swift.h>' < %t/Text.h
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only %t/mixed.cpp -I %t -DSWIFT_HEADER_FIRST
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only %t/mixed.cpp -I %t

// INLINE: #ifndef SWIFT_SWIFT_H
// INLINE: #ifndef SWIFT_PRINTED_CORE
// INLINE: class SWIFT_SYMBOL("s:SS") String

//--- Record.swift
public struct Item {
  public var value: Int

  public init(_ value: Int) {
    self.value = value
  }
}

public final class Owner {
  public let value: Int

  public init(_ value: Int) {
    self.value = value
  }
}

//--- Text.swift
public func greeting() -> String {
  return "hello"
}

@c(sharedAnswer)
public func answer() -> Int32 {
  return 42
}

//--- standalone.cpp
#include <swift/Swift.h>
#include <cassert>
#include <string>

int main() {
  swift::String message = "hello";
  message.append(" world");
  assert(std::string(message) == "hello world");

  auto numbers = swift::Array<swift::Int>::init();
  numbers.append(42);
  assert(numbers.getCount() == 1);
  assert(numbers[0] == 42);

  auto value = swift::Optional<swift::String>::some(message);
  assert(value.isSome());
  assert(std::string(value.get()) == "hello world");
  assert(swift::Optional<swift::String>::none().isNone());
}

//--- mixed.cpp
#ifdef SWIFT_HEADER_FIRST
#include <swift/Swift.h>
#endif
#include "Record.h"
#include "Text.h"
#include <swift/Swift.h>
#include <cassert>
#include <string>

int main() {
  auto items = swift::Array<Record::Item>::init();
  items.append(Record::Item::init(42));
  assert(items.getCount() == 1);
  assert(items[0].getValue() == 42);
  auto owner = Record::Owner::init(7);
  auto copy = owner;
  assert(copy.getValue() == 7);
  assert(std::string(Text::greeting()) == "hello");
  assert(sharedAnswer() == 42);
}
