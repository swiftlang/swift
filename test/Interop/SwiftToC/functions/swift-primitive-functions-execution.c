// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-primitive-functions-c-bridging.swift -module-name Functions -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clang -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-primitive-functions-c-bridging.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: executable_test

#include <assert.h>
#include "functions.h"

#define VERIFY_PASSTHROUGH_VALUE(function, value) assert(function(value) == (value));

int main() {
  // passThroughCBool
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughCBoolyS2bF, true);

  // passThroughCChar
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughCCharys4Int8VADF, 'a');
  // passThroughCWideChar
#if defined(_WIN32)
  VERIFY_PASSTHROUGH_VALUE($s9Functions20passThroughCWideCharys6UInt16VADF, 'a');
#else
  VERIFY_PASSTHROUGH_VALUE($s9Functions20passThroughCWideCharys7UnicodeO6ScalarVAFF, 'a');
#endif
  // passThroughCChar16
  VERIFY_PASSTHROUGH_VALUE($s9Functions18passThroughCChar16ys6UInt16VADF, 0xFE1);
  // passThroughCChar32
  VERIFY_PASSTHROUGH_VALUE($s9Functions18passThroughCChar32ys7UnicodeO6ScalarVAFF, 0x100FE);

  // passThroughCSignedChar
  VERIFY_PASSTHROUGH_VALUE($s9Functions22passThroughCSignedCharys4Int8VADF, -1);
  // passThroughCShort
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughCShortys5Int16VADF, -512);
  // passThroughCInt
  VERIFY_PASSTHROUGH_VALUE($s9Functions15passThroughCIntys5Int32VADF, -999999);
  // passThroughCLongLong
  VERIFY_PASSTHROUGH_VALUE($s9Functions20passThroughCLongLongys5Int64VADF, -999998);

  // passThroughCUnsignedSignedChar
  VERIFY_PASSTHROUGH_VALUE($s9Functions30passThroughCUnsignedSignedCharys5UInt8VADF, 255);
  // passThroughCUnsignedShort
  VERIFY_PASSTHROUGH_VALUE($s9Functions25passThroughCUnsignedShortys6UInt16VADF, 0xFFFF);
  // passThroughCUnsignedInt
  VERIFY_PASSTHROUGH_VALUE($s9Functions23passThroughCUnsignedIntys6UInt32VADF, 0xFFFFFFFF);
  // passThroughCUnsignedLongLong
  VERIFY_PASSTHROUGH_VALUE($s9Functions024passThroughCUnsignedLongE0ys6UInt64VADF, 0xFFFFFFFF);

  // passThrougCFloat
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughCFloatyS2fF, 1.0f);
  // passThroughCDouble
  VERIFY_PASSTHROUGH_VALUE($s9Functions18passThroughCDoubleyS2dF, 42.125f);

  // passThroughInt8
  VERIFY_PASSTHROUGH_VALUE($s9Functions15passThroughInt8ys0D0VADF, -1);
  // passThroughInt16
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughInt16ys0D0VADF, -512);
  // passThroughInt32
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughInt32ys0D0VADF, -999999);
  // passThroughInt64
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughInt64ys0D0VADF, -999999999999);

  // passThroughUInt8
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughUInt8ys0D0VADF, 255);
  // passThroughUInt16
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughUInt16ys0D0VADF, 0xffff);
  // passThroughUInt32
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughUInt32ys0D0VADF, 0xffffffff);
  // passThroughUInt64
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughUInt64ys0D0VADF, 0xffffffffffffffff);

  // passThroughFloat
  VERIFY_PASSTHROUGH_VALUE($s9Functions16passThroughFloatyS2fF, 1.0f);
  // passThroughDouble
  VERIFY_PASSTHROUGH_VALUE($s9Functions17passThroughDoubleyS2dF, 42.125f);
  // passThroughFloat32
  VERIFY_PASSTHROUGH_VALUE($s9Functions18passThroughFloat32yS2fF, 1.0f);
  // passThroughFloat64
  VERIFY_PASSTHROUGH_VALUE($s9Functions18passThroughFloat64yS2dF, 42.125f);

  // passThroughInt
  VERIFY_PASSTHROUGH_VALUE($s9Functions14passThroughIntyS2iF, -999997);
  // passThroughUInt
  VERIFY_PASSTHROUGH_VALUE($s9Functions15passThroughUIntyS2uF, 0xffffffff);
  // passThroughBool
  VERIFY_PASSTHROUGH_VALUE($s9Functions15passThroughBoolyS2bF, true);

  int x = 0;
  // passThroughOpaquePointer
  VERIFY_PASSTHROUGH_VALUE($s9Functions24passThroughOpaquePointerys0dE0VADF, &x);
  // passThroughUnsafeRawPointer
  VERIFY_PASSTHROUGH_VALUE($s9Functions27passThroughUnsafeRawPointeryS2VF, &x);
  // passThroughUnsafeMutableRawPointer
  VERIFY_PASSTHROUGH_VALUE($s9Functions34passThroughUnsafeMutableRawPointeryS2vF, &x);
}
