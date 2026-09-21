// RUN: %empty-directory(%t)
// RUN: %{python} %S/Inputs/many_extensions.py 3000 > %t/lib.swift
// RUN: %target-swift-frontend -emit-module -module-name ManyExtLib -o %t/ManyExtLib.swiftmodule %t/lib.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

import ManyExtLib

func use(_ x: Outer.Inner) -> Int {
  return x.f2000() + x.f3000()
}
