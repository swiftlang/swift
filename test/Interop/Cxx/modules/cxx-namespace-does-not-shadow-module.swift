// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/lib)
// RUN: %empty-directory(%t/include)
// RUN: split-file %s %t

// Compile shim.swift in C mode, then re-compile it in C++ mode:
//
// RUN: %target-swift-emit-module-interface(%t/lib/shim.swiftinterface) %t/shim.swift -module-name shim -I %t/include
// RUN: %FileCheck %t/shim.swift < %t/lib/shim.swiftinterface
// RUN: %swift-frontend %t/program.swift -typecheck -verify -cxx-interoperability-mode=default -I %t/include -I %t/lib

// Compile cxxshim.swift in C++ mode, check that its interface is still usable
// (despite using a mix of C/C++ decls):
//
// RUN: %empty-directory(%t/lib)
// RUN: %target-swift-emit-module-interface(%t/lib/shim.swiftinterface) -cxx-interoperability-mode=default %t/cxxshim.swift -module-name shim -I %t/include
// RUN: %FileCheck %t/cxxshim.swift < %t/lib/shim.swiftinterface
// RUN: %swift-frontend %t/program.swift -typecheck -verify -cxx-interoperability-mode=default -I %t/include -I %t/lib

//--- include/module.modulemap
// A Clang module which will first be compiled in C mode, and then later compiled in C++ mode
module c2cxx {
  header "c2cxx.h"
  export *
}

//--- include/c2cxx.h
// A C/C++ header file that defines a namespace with the same name as the module, a common idiom
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
public func shimId(_ n: c2cxx_number) -> c2cxx_number { return n }
// CHECK: public func shimId(_ n: c2cxx.c2cxx_number) -> c2cxx.c2cxx_number
//                                ^^^^^`- refers to the module

//--- cxxshim.swift
// Another shim around c2cxx that refers to a mixture of namespaced (C++) and
// top-level (C) decls; requires cxx-interoperability-mode
import c2cxx
public func shimId(_ n: c2cxx.number) -> c2cxx_number { return n }
//                      ^^^^^`- refers to the namespace
// CHECK: public func shimId(_ n: c2cxx.c2cxx.number) -> c2cxx.c2cxx_number
//                                ^^^^^\^^^^^`-namespace ^^^^^`-module
//                                      `-module

//--- program.swift
// Uses the shim and causes it to be (re)built from its interface
import shim
func numberwang() { let _ = shimId(42) }
