// RUN: rm -rf %t && mkdir -p %t

// This check uses -parse-stdlib in order to have an exact count of declarations
// imported.
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_xref_extensions.swift -parse-stdlib
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_xref_extensions_distraction.swift -parse-stdlib
// RUN: %target-swift-frontend -I %t -typecheck %s -parse-stdlib -print-stats 2>&1 -D CHECK_NESTED | %FileCheck %s -check-prefix CHECK_NESTED
// RUN: %target-swift-frontend -I %t -typecheck %s -parse-stdlib -print-stats 2>&1 | %FileCheck %s -check-prefix CHECK_NON_NESTED

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_xref_extensions.swift -parse-stdlib -DEXTRA
// RUN: %target-swift-frontend -I %t -typecheck %s -parse-stdlib -print-stats 2>&1 -D CHECK_NESTED | %FileCheck %s -check-prefix CHECK_NESTED
// RUN: %target-swift-frontend -I %t -typecheck %s -parse-stdlib -print-stats 2>&1 | %FileCheck %s -check-prefix CHECK_NON_NESTED

// REQUIRES: asserts

// CHECK_NESTED-LABEL: Statistics
// CHECK_NESTED: 9 Serialization - # of decls deserialized
// outer struct, initializer + self param,
// inner struct, initializer + self param,
// extension, func + self param

// CHECK_NON_NESTED-LABEL: Statistics
// CHECK_NON_NESTED: 6 Serialization - # of decls deserialized
// struct, initializer + self param, extension, func + self param

import def_xref_extensions
import def_xref_extensions_distraction

#if CHECK_NESTED
Outer.InterestingValue.foo()
#else
def_xref_extensions_distraction.InterestingValue.bar()
#endif
