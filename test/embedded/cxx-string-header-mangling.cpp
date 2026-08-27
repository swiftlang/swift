// Exposing Swift's String to C++ from an Embedded Swift module must emit
// embedded-mangled symbols ($e prefix) into the generated header, must not fall
// back to the regular mangling ($s prefix), and must not pull in Foundation's
// String extensions.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded %t/Stringer.swift -target %target-cpu-apple-macos15.0 -module-name Stringer -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Stringer.h

// Verify the generated header uses embedded mangling ($e prefix, not $s).
// RUN: %FileCheck %s --check-prefix=CHECK-HEADER < %t/Stringer.h

// Verify the generated header compiles as valid C++.
// RUN: %target-interop-build-clangxx -target %target-cpu-apple-macos15.0 -std=gnu++20 -c %t/main.cpp -I %t -o %t/main.o

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// CHECK-HEADER: __EmbeddedSwift__
// CHECK-HEADER: $eSS7cStringSSSPys4Int8VG_tcfC
// CHECK-HEADER-NOT: $sSS7cStringSSSPys4Int8VG_tcfC
// CHECK-HEADER-NOT: $sSS10FoundationE

//--- Stringer.swift

// Takes and returns String, so the generated header must emit swift::String
// along with its C-string bridging constructor.
@_expose(Cxx)
public func roundTrip(_ s: String) -> String {
    return s
}

//--- main.cpp

#include "Stringer.h"

int main() {
  // Exercises swift::String's const char * constructor, i.e. the
  // String.init(cString:) symbol checked for above.
  swift::String s = "hello world";
  swift::String result = Stringer::roundTrip(s);
  (void)result;
  return 0;
}
