// Test that @cxx requires both C++ interoperability and the CxxImplementation
// experimental feature.

// RUN: not %target-swift-frontend \
// RUN:   -typecheck %s \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   2>&1 | %FileCheck --check-prefix=NO-INTEROP %s

// RUN: not %target-swift-frontend \
// RUN:   -typecheck %s \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   2>&1 | %FileCheck --check-prefix=NO-FEATURE %s

// REQUIRES: swift_feature_CxxImplementation

@cxx @implementation
func foo(_ x: Int32) -> Int32 { return x }

// NO-INTEROP: error: 'cxx' requires C++ interoperability; enable it with '-cxx-interoperability-mode=default'
// NO-FEATURE: error: 'cxx' attribute is only valid when experimental feature CxxImplementation is enabled
