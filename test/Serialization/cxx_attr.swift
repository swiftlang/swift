// The @cxx attribute (and the @implementation match it drives) must survive
// serialization: compiling from a .sib must produce the same C++ entry point
// as compiling from source.

// RUN: %empty-directory(%t)

// Ensure .swift -> .ll
// RUN: %target-swift-frontend \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs/cxx-attr \
// RUN:   -emit-ir %s | %FileCheck %s

// Ensure .swift -> .sib -> .ll
// RUN: %target-swift-frontend \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs/cxx-attr \
// RUN:   -emit-sib %s -o %t/cxx_attr.sib
// RUN: %target-swift-frontend \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs/cxx-attr \
// RUN:   -emit-sil %t/cxx_attr.sib | %FileCheck --check-prefix=SIL %s

// REQUIRES: swift_feature_CxxImplementation

import CxxAttr

// CHECK: define {{.*}}@{{_Z3fooi|"\?foo@@YAHH@Z"}}

// SIL: [asmname "{{_Z3fooi|\?foo@@YAHH@Z}}"]

@cxx @implementation
func foo(_ x: Int32) -> Int32 { return x }
