// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/struct_with_operators.swift -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/alias.swift -module-name has_alias -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/xref_distraction.swift -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/has_xref.swift -swift-version 3
// RUN: llvm-bcanalyzer %t/has_xref.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %s -swift-version 3 > /dev/null

// CHECK-NOT: UnknownCode

import xref_distraction
import has_xref

numeric(42)
conditional(true)
conditional2(true)
longInt(42)
numericArray([42])

func incr<T: ExtraIncrementable>(_ x: T) {}
incr(SpecialInt())
