// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-primitive-functions-cxx-bridging.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-primitive-functions-cxx-bridging.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

#define VERIFY_PASSTHROUGH_VALUE(function, value) assert(function(value) == (value));

int main() {
  using namespace Functions;

  VERIFY_PASSTHROUGH_VALUE(passThroughCBool, true);

  VERIFY_PASSTHROUGH_VALUE(passThroughCChar, 'a');
  VERIFY_PASSTHROUGH_VALUE(passThroughCWideChar, 'a');
  VERIFY_PASSTHROUGH_VALUE(passThroughCChar16, 0xFE1);
  VERIFY_PASSTHROUGH_VALUE(passThroughCChar32, 0x100FE);

  VERIFY_PASSTHROUGH_VALUE(passThroughCSignedChar, -1);
  VERIFY_PASSTHROUGH_VALUE(passThroughCShort, -512);
  VERIFY_PASSTHROUGH_VALUE(passThroughCInt, -999999);
  VERIFY_PASSTHROUGH_VALUE(passThroughCLongLong, -999998);

  VERIFY_PASSTHROUGH_VALUE(passThroughCUnsignedSignedChar, 255);
  VERIFY_PASSTHROUGH_VALUE(passThroughCUnsignedShort, 0xFFFF);
  VERIFY_PASSTHROUGH_VALUE(passThroughCUnsignedInt, 0xFFFFFFFF);
  VERIFY_PASSTHROUGH_VALUE(passThroughCUnsignedLongLong, 0xFFFFFFFF);

  VERIFY_PASSTHROUGH_VALUE(passThroughCFloat, 1.0f);
  VERIFY_PASSTHROUGH_VALUE(passThroughCDouble, 42.125f);

  VERIFY_PASSTHROUGH_VALUE(passThroughInt8, -1);
  VERIFY_PASSTHROUGH_VALUE(passThroughInt16, -512);
  VERIFY_PASSTHROUGH_VALUE(passThroughInt32, -999999);
  VERIFY_PASSTHROUGH_VALUE(passThroughInt64, -999999999999);

  VERIFY_PASSTHROUGH_VALUE(passThroughUInt8, 255);
  VERIFY_PASSTHROUGH_VALUE(passThroughUInt16, 0xffff);
  VERIFY_PASSTHROUGH_VALUE(passThroughUInt32, 0xffffffff);
  VERIFY_PASSTHROUGH_VALUE(passThroughUInt64, 0xffffffffffffffff);

  VERIFY_PASSTHROUGH_VALUE(passThroughFloat, 1.0f);
  VERIFY_PASSTHROUGH_VALUE(passThroughDouble, 42.125f);
  VERIFY_PASSTHROUGH_VALUE(passThroughFloat32, 1.0f);
  VERIFY_PASSTHROUGH_VALUE(passThroughFloat64, 42.125f);

  VERIFY_PASSTHROUGH_VALUE(passThroughInt, -999997);
  VERIFY_PASSTHROUGH_VALUE(passThroughUInt, 0xffffffff);
  VERIFY_PASSTHROUGH_VALUE(passThroughBool, true);
  VERIFY_PASSTHROUGH_VALUE(passThroughBool, false);

  int x = 0;
  VERIFY_PASSTHROUGH_VALUE(passThroughOpaquePointer, &x);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeRawPointer, &x);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeMutableRawPointer, &x);
  VERIFY_PASSTHROUGH_VALUE(roundTwoPassThroughUnsafeMutableRawPointer, nullptr);

  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericMutableOptionalPointer, &x);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericMutableOptionalPointer,
                           nullptr);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericMutablePointer, &x);
  const int y = 0;
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericOptionalPointer, &x);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericOptionalPointer, nullptr);
  VERIFY_PASSTHROUGH_VALUE(passThroughUnsafeGenericPointer, &x);
}
