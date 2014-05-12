// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t %S/Inputs/struct_with_operators.swift
// RUN: %swift -emit-module -o %t %S/Inputs/alias.swift -module-name has_alias
// RUN: %swift -emit-module -o %t %S/Inputs/xref_distraction.swift
// RUN: %swift -emit-module -o %t -I=%t %S/Inputs/has_xref.swift
// RUN: llvm-bcanalyzer %t/has_xref.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -I=%t %s > /dev/null

// CHECK-NOT: UnknownCode

import xref_distraction
import has_xref

numeric(42)
conditional(true)
longInt(42)
numericArray([42])

var incr: ExtraIncrementable = SpecialInt()
