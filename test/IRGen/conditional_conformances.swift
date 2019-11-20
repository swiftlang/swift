// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_basic_conformances.swift | %FileCheck %S/../Inputs/conditional_conformance_basic_conformances.swift --check-prefix=CHECK --check-prefix=%target-os
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_with_assoc.swift | %FileCheck %S/../Inputs/conditional_conformance_with_assoc.swift --check-prefix=CHECK --check-prefix=%target-os
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_subclass.swift | %FileCheck %S/../Inputs/conditional_conformance_subclass.swift --check-prefix=CHECK --check-prefix=%target-os
// RUN: %target-swift-frontend -emit-ir %S/../Inputs/conditional_conformance_recursive.swift | %FileCheck %S/../Inputs/conditional_conformance_recursive.swift --check-prefix=CHECK --check-prefix=%target-os

// Too many pointer-sized integers in the IR
// REQUIRES: PTRSIZE=64

