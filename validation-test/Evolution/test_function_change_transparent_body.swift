// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -whole-module-optimization -parse-as-library -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/function_change_transparent_body.swift -o %t/before/function_change_transparent_body.o
// RUN: %target-build-swift -whole-module-optimization -parse-as-library -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/function_change_transparent_body.swift -o %t/before/function_change_transparent_body.o

// RUN: %target-build-swift -whole-module-optimization -parse-as-library -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/function_change_transparent_body.swift -o %t/after/function_change_transparent_body.o
// RUN: %target-build-swift -whole-module-optimization -parse-as-library -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/function_change_transparent_body.swift -o %t/after/function_change_transparent_body.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/function_change_transparent_body.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/function_change_transparent_body.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/function_change_transparent_body.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/function_change_transparent_body.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

// REQUIRES: executable_test

// FIXME: shouldn't need -whole-module-optimization here; we need to fix the
// frontend to merge serialized SIL functions from different translation units

import StdlibUnittest
import function_change_transparent_body

var ChangeTransparentBodyTest = TestSuite("ChangeTransparentBody")

ChangeTransparentBodyTest.test("ChangeTransparentBody") {

#if BEFORE
  expectEqual(getBuildVersion(), 0)
#else
  expectEqual(getBuildVersion(), 1)
#endif

}

runAllTests()
