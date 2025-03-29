// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t

// Generate textual interface for shim.swift without C++ interop enabled, then
// re-compile it with C++ interop enabled (because it is used by program.swift,
// which is compiled with C++ interop enabled).
//
// RUN: %empty-directory(%t/lib)
// RUN: %target-swift-emit-module-interface(%t/lib/shim.swiftinterface) %t/shim.swift -module-name shim -I %t/include
// RUN: %FileCheck %t/shim.swift < %t/lib/shim.swiftinterface
// RUN: %target-swift-frontend %t/program.swift -typecheck -verify -cxx-interoperability-mode=default -I %t/include -I %t/lib

//--- include/module.modulemap
// A Clang module which will first be compiled in C mode, and then later compiled in C++ mode
module c2cxx {
  header "c2cxx.h"
  export *
}

//--- include/c2cxx.h
// A header file that defines a namespace with the same name as the module,
// a common C++ idiom. We want to make sure that it does not shadow the module
// in textual interfaces generated with C++ interop disabled, but later compiled
// with C++ interop enabled.
#ifndef __C2CXX_NAMESPACE_H
#define __C2CXX_NAMESPACE_H
typedef int c2cxx_number;                         // always available and resilient
#ifdef __cplusplus
namespace c2cxx { typedef c2cxx_number number; }; // only available in C++
#endif // __cplusplus
#endif // __C2CXX_NAMESPACE_H

//--- shim.swift
// A shim around c2cxx that exposes a c2cxx decl in its module interface
import c2cxx

// Exposes a (fully-qualified) c2cxx decl in its module interface.
public func shimId(_ n: c2cxx_number) -> c2cxx_number { return n }
// CHECK: public func shimId(_ n: c2cxx.c2cxx_number) -> c2cxx.c2cxx_number
//                                ^^^^^`- refers to the module

// @inlinable functions have their bodies splatted in the module interface file;
// those verbatim bodies may contain unqualified names.
@inlinable public func shimIdInline(_ n: c2cxx_number) -> c2cxx_number {
// CHECK:  public func shimIdInline(_ n: c2cxx.c2cxx_number) -> c2cxx.c2cxx_number
//                                       ^^^^^`- refers to the module
          let m: c2cxx_number = n
// CHECK: let m: c2cxx_number = n
//               ^^^^^^^^^^^^`- verbatim (unqualified)
          return m
}

//--- program.swift
// Uses the shim and causes it to be (re)built from its interface
import shim
func numberwang() { let _ = shimId(42) + shimIdInline(24) }
