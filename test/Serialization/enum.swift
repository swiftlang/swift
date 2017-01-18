// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -module-name def_enum -o %t %S/Inputs/def_enum.swift %S/Inputs/def_enum_derived.swift
// RUN: llvm-bcanalyzer %t/def_enum.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -I %t %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -I %t %s -o /dev/null

// CHECK-NOT: UnknownCode

import def_enum

extension Basic {
  init(silly: Int) {
    self.init()
    self = .HasType(silly)
  }
}

var a : Basic
a = .Untyped
a.doSomething()
a = .HasType(4)
a.doSomething()

var g = Generic.Left(false)
g = .Right(true)

var lazy = Lazy.Thunk({ 42 })
var comp : Computable = lazy
comp.compute()
lazy.compute()

struct Tea {}

let meal = Breakfast<Basic>.Bacon
let n = meal.rawValue

do { throw meal } catch {}
