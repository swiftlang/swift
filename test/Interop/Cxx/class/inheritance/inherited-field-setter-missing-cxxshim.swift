// Regression test for a SILGen crash when the synthesized accessor of an
// inherited C++ base class data member is emitted while CxxShim is not loaded,
// which was historically needed to provide the upcast used by those accessors.
// Emitting a module interface disables the implicit CxxShim import.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/main.swift -module-name Test \
// RUN:     -I %t/Inputs -cxx-interoperability-mode=default -swift-version 5 \
// RUN:     -emit-module-interface-path %t/Test.swiftinterface \
// RUN:     -o %t/Test.swiftmodule

// RUN: %target-swift-frontend -emit-silgen %t/main.swift -module-name Test \
// RUN:     -I %t/Inputs -cxx-interoperability-mode=default -swift-version 5 \
// RUN: | %FileCheck %s

//--- Inputs/module.modulemap
module Minimal {
  header "minimal.hpp"
  requires cplusplus
}

//--- Inputs/minimal.hpp
#pragma once

struct Base { double value; };
struct Derived : public Base {};

//--- main.swift
import Minimal

// Assigning through the inherited data member forces SILGen to emit the
// synthesized Derived.value setter.
func writeThroughInheritedField(_ d: inout Derived) { d.value = 1.0 }

// CHECK: function_ref @{{.*}}__swift_interopStaticCast_{{.*}}Derived{{.*}}_to_{{.*}}Base{{.*}} : $@convention(c) (UnsafeMutablePointer<Derived>) -> UnsafeMutablePointer<Base>
