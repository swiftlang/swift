// RUN: %empty-directory(%t)

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

// FIXME: CHECK_NESTED use to deserialize only 4 decls,
//        and CHECK_NON_NESTED only 3. My guess is that the two new
//        decls deserialized are Copyable & Escapable, since they get
//        synthesized into the Builtin module when missing?? Or, it has to
//        do with the new Copyable/Escapable requirements on the extensions.
//        rdar://124555449

// CHECK_NESTED-LABEL: Statistics
// CHECK_NESTED: 6 Serialization - # of decls deserialized
// outer struct, inner struct, extension, func + self param

// CHECK_NON_NESTED-LABEL: Statistics
// CHECK_NON_NESTED: 5 Serialization - # of decls deserialized
// struct, extension, func + self param

import def_xref_extensions
import def_xref_extensions_distraction

#if CHECK_NESTED
Outer.InterestingValue.foo()
#else
def_xref_extensions_distraction.InterestingValue.bar()
#endif
