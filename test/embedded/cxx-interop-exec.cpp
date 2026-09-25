// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/embedded-swift.swift -target %target-cpu-apple-macosx15.0 -module-name Core -swift-version 6 -enable-experimental-feature Embedded -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-embedded-execution.o -fno-exceptions -fno-rtti
// RUN: %target-interop-build-swift -target %target-cpu-apple-macosx15.0 -wmo %S/embedded-swift.swift -o %t/swift-embedded-execution -Xlinker %t/swift-embedded-execution.o -module-name Core -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature Embedded -Xcc -fno-rtti -Xcc -fno-exceptions -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a %target-embedded-posix-shim

// RUN: %target-codesign %t/swift-embedded-execution
// RUN: %target-run %t/swift-embedded-execution

// swift::Error links in Embedded mode, and what() returns a placeholder there.
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-embedded-error.o \
// RUN:   -fno-exceptions -fno-rtti -DSWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
// RUN: %target-interop-build-swift -target %target-cpu-apple-macosx15.0 -wmo \
// RUN:   %S/embedded-swift.swift -o %t/swift-embedded-error \
// RUN:   -Xlinker %t/swift-embedded-error.o -module-name Core \
// RUN:   -Xfrontend -entry-point-function-name -Xfrontend swiftMain \
// RUN:   -enable-experimental-feature Embedded \
// RUN:   -Xcc -fno-rtti -Xcc -fno-exceptions \
// RUN:   %embedded-unicode-tables %target-embedded-posix-shim
// RUN: %target-codesign %t/swift-embedded-error
// RUN: %target-run %t/swift-embedded-error | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: executable_test
// REQUIRES: CPU=arm64e || CPU=arm64

#include "core.h"
#include <assert.h>
#include <stdio.h>

int main() {
  assert(Core::id(5) == 5);
#ifdef SWIFT_CXX_INTEROP_EXPERIMENTAL_SWIFT_ERROR
  swift::Error error;
  const std::exception &exception = error;
  puts(exception.what());
  // CHECK: swift::Error
#endif
  return 0;
}
