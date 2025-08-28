// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/embedded-swift.swift -target arm64-apple-macosx15.0 -module-name Core -swift-version 6 -enable-experimental-feature Embedded -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-embedded-execution.o -fno-exceptions -fno-rtti
// RUN: %target-interop-build-swift -target arm64-apple-macosx15.0 -wmo %S/embedded-swift.swift -o %t/swift-embedded-execution -Xlinker %t/swift-embedded-execution.o -module-name Core -Xfrontend -entry-point-function-name -Xfrontend swiftMain -enable-experimental-feature Embedded -Xcc -fno-rtti -Xcc -fno-exceptions -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a

// RUN: %target-codesign %t/swift-embedded-execution
// RUN: %target-run %t/swift-embedded-execution

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: executable_test
// REQUIRES: CPU=arm64e || CPU=arm64

#include <assert.h>
#include "core.h"

int main() {
  assert(Core::id(5) == 5);
  return 0;
}
