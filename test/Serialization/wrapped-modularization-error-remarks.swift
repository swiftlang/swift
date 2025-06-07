// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/wrapped-modularization-error-remarks/A/A.swift -cxx-interoperability-mode=default -Xcc -fmodule-map-file=%S/Inputs/wrapped-modularization-error-remarks/CxxLib/include/module.modulemap -Xcc -I -Xcc %S/Inputs/wrapped-modularization-error-remarks/CxxLib/include -module-name A -o %t/A.swiftmodule
// RUN: not %target-swift-frontend -c -primary-file %S/Inputs/wrapped-modularization-error-remarks/B/B.swift -cxx-interoperability-mode=default -I %t -Xcc -fmodule-map-file=%S/Inputs/wrapped-modularization-error-remarks/CxxLib/include/module.modulemap -Xcc -I -Xcc %S/Inputs/wrapped-modularization-error-remarks/CxxLib/include -module-name B -o %t/B.swift.o -Rmodule-recovery 2>&1 | %FileCheck %s
// REQUIRES: OS=windows-msvc

// Check that the diagnostics/remark from the wrapped ModularizationError is emitted.
// CHECK: remark: reference to type '_GUID' broken by a context change; '_GUID' is not found, it was expected to be in 'CxxLib'
// CHECK: note: could not deserialize type for 'queryInterface'
// CHECK: error: cannot inherit from class 'Window' because it has overridable members that could not be loaded
// CHECK: note: could not deserialize 'queryInterface'
