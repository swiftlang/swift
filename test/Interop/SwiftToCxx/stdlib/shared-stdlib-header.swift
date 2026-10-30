// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %check-interop-cxx-header-in-clang(%test-resource-dir/swift/Swift.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY -Wno-unused-private-field -Wno-unused-function)

// A module with no stdlib wrapper in its interface must also use the shared
// header, so its consumers can instantiate stdlib templates with its types.
// RUN: %target-swift-frontend %t/Record.swift -module-name Record -typecheck -verify -cxx-interoperability-mode=default -emit-clang-header-path %t/Record.h
// RUN: %FileCheck %s --check-prefix=RECORD --implicit-check-not=SWIFT_PRINTED_CORE --implicit-check-not=SWIFT_SWIFT_H --implicit-check-not=_SwiftCxxInteroperability.h < %t/Record.h
// RUN: %target-swift-frontend %t/Text.swift -module-name Text -typecheck -verify -cxx-interoperability-mode=default -emit-clang-header-path %t/Text.h
// RUN: %FileCheck %s --check-prefix=TEXT --implicit-check-not=SWIFT_PRINTED_CORE --implicit-check-not=SWIFT_SWIFT_H --implicit-check-not=_SwiftCxxInteroperability.h < %t/Text.h
// RUN: %check-interop-cxx-header-in-clang(%t/Record.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(%t/Text.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// C and Objective-C clients do not need the Swift standard library header or
// its include path, even when the module was built with C++ interoperability.
// RUN: %check-interop-c-header-in-clang(%t/Record.h)
// RUN: %check-interop-c-header-in-clang(%t/Text.h)
// RUN: %target-clang -x c -std=c11 -Werror=implicit-function-declaration -fsyntax-only %t/client.c -I %t -I %t/no-swift
// RUN: %if objc_interop %{ %target-swift-frontend %t/Object.swift -module-name Object -typecheck -verify -cxx-interoperability-mode=default -emit-clang-header-path %t/Object.h %}
// RUN: %if objc_interop %{ %target-clang -x objective-c -Werror=implicit-function-declaration -Werror=objc-method-access -fsyntax-only %t/client.m -I %t -I %t/no-swift %}
// RUN: %if objc_interop %{ %target-interop-build-clangxx -x objective-c++ -std=c++17 -Werror=objc-method-access -fsyntax-only %t/client.m -I %t %}

// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only %t/mixed.cpp -I %t -DSWIFT_HEADER_FIRST
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only %t/mixed.cpp -I %t

// RECORD: #include <swift/Swift.h>
// RECORD: namespace Record SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Record") {
// RECORD: class SWIFT_SYMBOL("s:6Record4ItemV") Item final {

// TEXT: #include <swift/Swift.h>
// TEXT: SWIFT_EXTERN int32_t sharedAnswer(void) SWIFT_NOEXCEPT SWIFT_WARN_UNUSED_RESULT;
// TEXT: namespace Text SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Text") {
// TEXT: swift::String greeting()

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

//--- Object.swift
import Foundation

@objc public class SharedObject: NSObject {
  @objc public func answer() -> Int32 {
    return 42
  }
}

//--- client.c
#include "Text.h"

int main(void) {
  return sharedAnswer() == 42 ? 0 : 1;
}

//--- client.m
#include "Object.h"
#include "Text.h"

int32_t answer(SharedObject *object) {
  return [object answer] + sharedAnswer();
}

#ifdef __cplusplus
swift::String greeting(SharedObject *object) {
  return [object answer] == 42 ? "hello" : "goodbye";
}
#endif

//--- no-swift/swift/Swift.h
#error C and Objective-C clients must not include the Swift standard library header.

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
