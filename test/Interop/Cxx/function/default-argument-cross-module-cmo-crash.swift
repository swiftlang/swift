// Reproducer for a SIL cross-module deserialization crash with C++ interop.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Producer: build Lib.swiftmodule with C++ interop and default CMO so that the
// synthesized C++ default-argument thunk is serialized into its SIL.
// RUN: %target-build-swift -O -wmo -Xfrontend -enable-default-cmo \
// RUN:   -cxx-interoperability-mode=default -parse-as-library \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule -module-name Lib \
// RUN:   -I %t/Inputs %t/Lib.swift -c -o %t/lib.o

// Consumer: compiling Main synthesizes the same thunk and, via
// MandatorySILLinker, must deserialize Lib's serialized copy of it.
// RUN: %target-build-swift -O -wmo -Xfrontend -enable-default-cmo \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -module-name Main -I %t -I %t/Inputs %t/Main.swift -emit-sil

//--- Inputs/module.modulemap
module CxxLib {
  header "cxxlib.h"
  requires cplusplus
  export *
}

//--- Inputs/cxxlib.h
#pragma once

// A free C++ function with a single defaulted parameter. Omitting `m` at a
// Swift callsite triggers synthesis of __cxx__defaultArg_0__Z4make...
inline int make(int m = 0) {
  (void)m;
  return 0;
}

//--- Lib.swift
import CxxLib

// Omitting the default synthesizes the thunk. With default CMO this function's
// SIL (referencing the thunk, whose parent is recorded as __ObjC) is serialized
// into Lib.swiftmodule.
public func go() -> Int32 {
  return make()
}

//--- Main.swift
import CxxLib
import Lib

// Main does not reference anything in Lib; importing Lib loads its serialized
// thunk body, and calling make() here synthesizes the same thunk symbol.
public func run() -> Int32 {
  return make()
}
