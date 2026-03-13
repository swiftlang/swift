// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-primitive-functions-c-bridging-long-lp64.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clang -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-primitive-functions-c-bridging-long-lp64.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: executable_test

#include <assert.h>
#include "functions.h"

#define VERIFY_PASSTHROUGH_VALUE(function, value) assert(function(value) == (value));

int main() {
  // passThroughCLong
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughCLongyS2iF, -999999);

  // passThroughCUnsignedLong
  VERIFY_PASSTHROUGH_VALUE($s9Functions24passThroughCUnsignedLongyS2uF, 0xFFFFFFFF);
}
