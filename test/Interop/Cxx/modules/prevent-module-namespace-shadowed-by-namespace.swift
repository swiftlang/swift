// XFAIL: *
// This test currently fails because there's no way to explicitly refer to
// a module that has been shadowed by another declaration, e.g., a namespace.
// Unlike in the prevent-module-shadowed-by-namespace.swift test, the textual
// interface is generated with C++ interop enabled, which means namespaces are
// not filtered out during name lookup when the interface is recompiled later.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t

// Compile shim.swift with C++ interop, check that its interface is usable
// (despite using a mix of C/C++ decls):
//
// RUN: %empty-directory(%t/lib)
// RUN: %target-swift-emit-module-interface(%t/lib/shim.swiftinterface) %t/shim.swift -cxx-interoperability-mode=default -module-name shim -I %t/include
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
// A shim around c2cxx that refers to a mixture of namespaced (C++) and
// top-level (C) decls; requires cxx-interoperability-mode
import c2cxx
public func shimId(_ n: c2cxx.number) -> c2cxx_number { return n }
//                      ^^^^^`- refers to the namespace
// CHECK: public func shimId(_ n: c2cxx.c2cxx.number) -> c2cxx.c2cxx_number
//                                ^^^^^\^^^^^`-namespace ^^^^^`-module
//                                      `-module

@inlinable public func shimIdInline(_ n: c2cxx_number) -> c2cxx.number {
// CHECK:  public func shimIdInline(_ n: c2cxx.c2cxx_number) -> c2cxx.c2cxx.number
//                                       ^^^^^`-module          ^^^^^\^^^^^`-namespace
//                                                                    `-module
          let m: c2cxx.number = n
// CHECK: let m: c2cxx.number = n
//               ^^^^^`-namespace
          return m
}

//--- program.swift
// Uses the shim and causes it to be (re)built from its interface
import shim
func numberwang() { let _ = shimId(42) + shimIdInline(24) }
