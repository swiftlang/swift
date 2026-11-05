// RUN: %empty-directory(%t)
// RUN: %{python} %S/Inputs/many_extensions.py 3000 > %t/lib.swift
// RUN: %target-swift-frontend -emit-module -module-name ManyExtLib -o %t/ManyExtLib.swiftmodule %t/lib.swift

// 13108 `==` operator methods (5 bytes each) and nested `N` types (8 bytes
// each) overflow a 16-bit dataLength in the decl and nested-type tables.
// RUN: %{python} %S/Inputs/many_same_name_decls.py 13108 > %t/same_name.swift
// RUN: %target-swift-frontend -emit-module -module-name SameNameLib -o %t/SameNameLib.swiftmodule %t/same_name.swift

// RUN: %target-swift-frontend -typecheck -I %t %s

import ManyExtLib
import SameNameLib

func use(_ x: Outer.Inner) -> Int {
  return x.f2000() + x.f3000()
}

func useSameName(_ a: S13107, _ b: S13107.N) -> Bool {
  return a == a
}
