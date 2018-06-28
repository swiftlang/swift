// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/struct_with_operators.swift -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/alias.swift -module-name has_alias -swift-version 3
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/has_xref.swift -swift-version 3
// RUN: llvm-bcanalyzer %t/has_xref.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t -primary-file %s %S/Inputs/xref-multi-file-other.swift -module-name main -swift-version 3 > /dev/null

// CHECK-NOT: UnknownCode

import has_xref

func use<T: DefaultInitializable>(_: T) {}
func test(x: SpecialInt) {
  use(x)
}
