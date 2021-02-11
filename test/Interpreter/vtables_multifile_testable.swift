// We test various combinations to make sure that -enable-testing does not
// break ABI with or without -enable-library-evolution.

////

// RUN: %empty-directory(%t)

// 1) -enable-testing OFF / -enable-library-evolution OFF

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -emit-module -emit-module-path %t/vtables_multifile_testable_helper.swiftmodule
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// 2) -enable-testing ON / -enable-library-evolution OFF

// ... first without rebuilding the client:

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -enable-testing -emit-module -emit-module-path %t/vtables_multifile_testable_helper.swiftmodule
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// ... now try to rebuild the client:

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

////

// Delete build artifacts
// RUN: %empty-directory(%t)

// 3) -enable-testing OFF / -enable-library-evolution ON

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -enable-library-evolution -emit-module -emit-module-path %t/vtables_multifile_testable_helper.swiftmodule
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// 4) -enable-testing ON / -enable-library-evolution ON

// ... first without rebuilding the client:

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -enable-testing -enable-library-evolution -emit-module -emit-module-path %t/vtables_multifile_testable_helper.swiftmodule
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// ... now try to rebuild the client:

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

////

// Delete build artifacts
// RUN: %empty-directory(%t)

// 5) -enable-testing OFF / -enable-library-evolution ON / textual interfaces

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -enable-library-evolution -emit-module-interface -emit-module-interface-path %t/vtables_multifile_testable_helper.swiftinterface
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// 6) -enable-testing ON / -enable-library-evolution ON / textual interfaces

// ... first without rebuilding the client:

// RUN: %target-build-swift-dylib(%t/%target-library-name(vtables_multifile_testable_helper)) %S/Inputs/vtables_multifile_testable_helper.swift -enable-testing -enable-library-evolution -emit-module-interface -emit-module-interface-path %t/vtables_multifile_testable_helper.swiftinterface
// RUN: %target-codesign %t/%target-library-name(vtables_multifile_testable_helper)

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)

// ... now try to rebuild the client:

// RUN: %target-build-swift %s -L %t -I %t -lvtables_multifile_testable_helper -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(vtables_multifile_testable_helper)


// REQUIRES: executable_test

import StdlibUnittest
import vtables_multifile_testable_helper

var VTableTestSuite = TestSuite("VTable")

public class Derived : Middle {
  public override func method() -> Int {
    return super.method() + 1
  }
}

VTableTestSuite.test("Derived") {
  expectEqual(3, callBaseMethod(Derived()))
  expectEqual(3, callMiddleMethod(Derived()))
}

runAllTests()
