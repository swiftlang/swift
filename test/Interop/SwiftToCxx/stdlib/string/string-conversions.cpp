// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/print-string.swift -module-name Stringer -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Stringer.h
// RUN: %FileCheck %s --check-prefix=CHECK-HEADER < %t/Stringer.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -c %t/string-conversions.cpp -I %t -o %t/swift-stdlib-execution.o
// RUN: %target-build-swift %t/print-string.swift -o %t/swift-stdlib-execution -Xlinker %t/swift-stdlib-execution.o -module-name Stringer -Xfrontend -entry-point-function-name -Xfrontend swiftMain %target-cxx-lib
// RUN: %target-codesign %t/swift-stdlib-execution
// RUN: %target-run %t/swift-stdlib-execution | %FileCheck %s

// RUN: %target-interop-build-clangxx -std=gnu++20 -O -c %t/string-conversions.cpp -I %t -o %t/swift-stdlib-execution-opt.o
// RUN: %target-build-swift -O %t/print-string.swift -o %t/swift-stdlib-execution-opt -Xlinker %t/swift-stdlib-execution-opt.o -module-name Stringer -Xfrontend -entry-point-function-name -Xfrontend swiftMain %target-cxx-lib
// RUN: %target-codesign %t/swift-stdlib-execution-opt
// RUN: %target-run %t/swift-stdlib-execution-opt | %FileCheck %s

// REQUIRES: executable_test

// CHECK-HEADER: SWIFT_EXTERN swift_interop_stub_Swift_String swift_stdlib_StringFromUTF8(const uint8_t * _Nonnull, size_t) SWIFT_NOEXCEPT SWIFT_CALL;

//--- print-string.swift

@_expose(Cxx)
public func printString(_ s: String) {
    print("'''\(s)'''")
}

@_expose(Cxx)
public func makeString(_ s: String, _ y: String) -> String {
    return "\(s)++\(y)"
}

//--- string-conversions.cpp

#include <cassert>
#include "Stringer.h"

// Check the bytes, including NULs, after constructing a Swift String in C++.
void checkConversion(const std::string &input, const std::string &expected) {
  auto s = swift::String(input);
  assert(s.getUtf8().getCount() == static_cast<swift::Int>(expected.size()));
  assert(static_cast<std::string>(s) == expected);

  // Also exercise the implicit conversion used by Swift function arguments.
  auto joined = Stringer::makeString(input, swift::String(""));
  assert(static_cast<std::string>(joined) == expected + "++");
}

int main() {
  using namespace swift;
  using namespace Stringer;

  {
    auto s = String("hello world");
    printString(s);
    swift::String s2 = "Hello literal";
    printString(s2);
    const char *literal = "Test literal via ptr";
    printString(literal);
    swift::String s3 = nullptr;
    printString(s3);
  }
// CHECK: '''hello world'''
// CHECK-NEXT: '''Hello literal'''
// CHECK-NEXT: '''Test literal via ptr'''
// CHECK-NEXT: ''''''

  {
    std::string str = "test std::string";
    printString(str);
  }
// CHECK-NEXT: '''test std::string'''
  {
    auto s = makeString(String("start"), String("end"));
    std::string str = s;
    assert(str == "start++end");
    str += "++cxx";
    printString(String(str));
  }
// CHECK-NEXT: '''start++end++cxx'''

  // std::string is counted UTF-8, including leading and trailing NULs.
  for (const auto &str : {std::string(), std::string("plain ASCII"),
                          std::string("a\0b", 3), std::string("\0ab", 3),
                          std::string("ab\0", 3), std::string("\0", 1),
                          std::string("\0\0\0", 3),
                          std::string("a\0b\0c\0", 6)}) {
    checkConversion(str, str);
  }

  // Exercise two-, three-, and four-byte UTF-8 sequences on both sides of NUL.
  const std::string unicode = "\xc3\xa9\xe2\x82\xac\xf0\x9f\x98\x80";
  checkConversion(unicode, unicode);
  const auto unicodeWithNul = unicode + std::string("\0", 1) + unicode;
  checkConversion(unicodeWithNul, unicodeWithNul);

  // Cover heap-backed strings as well as the small strings above.
  const auto longString = std::string(128, 'a') + std::string("\0", 1) +
                          std::string(128, 'b') + std::string("\0", 1);
  checkConversion(longString, longString);

  // Ill-formed UTF-8 must still be repaired, including beyond the first NUL.
  const std::string replacement = "\xef\xbf\xbd";
  checkConversion(std::string("a\0\xff" "b", 4),
                  std::string("a\0", 2) + replacement + "b");
  checkConversion(std::string("\xc3\0\xa9", 3),
                  replacement + std::string("\0", 1) + replacement);
  checkConversion(std::string("\xf0\x9f", 2), replacement);
  checkConversion(std::string("\xc0\xaf", 2), replacement + replacement);

  // const char* remains null-terminated UTF-8, with its existing repair rules.
  assert(static_cast<std::string>(String("a\0b")) == "a");
  assert(static_cast<std::string>(String("\0ab")) == "");
  assert(static_cast<std::string>(String("ab\0")) == "ab");
  assert(static_cast<std::string>(String("\xc3")) == replacement);
  assert(static_cast<std::string>(String(nullptr)) == "");
  return 0;
}
