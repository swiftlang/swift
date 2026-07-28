// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Producer: build Lib.swiftmodule with C++ interop and Embedded Swift so that
// the synthesized base getter accessor is serialized into its SIL.
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule \
// RUN:   -target %target-cpu-apple-macos14 -disable-availability-checking \
// RUN:   -enable-experimental-feature Embedded -wmo \
// RUN:   -cxx-interoperability-mode=default -parse-as-library \
// RUN:   -module-name Lib -I %t/Inputs %t/Lib.swift

// Consumer: compiling Main must deserialize Lib's serialized SIL, which
// cross-references the synthesized base getter accessor on Derived.
// RUN: %target-swift-frontend -emit-sil \
// RUN:   -target %target-cpu-apple-macos14 -disable-availability-checking \
// RUN:   -enable-experimental-feature Embedded -wmo \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -module-name Main -I %t -I %t/Inputs %t/Main.swift | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

//--- Inputs/module.modulemap
module CxxLib {
  header "cxxlib.h"
  requires cplusplus
  export *
}

//--- Inputs/cxxlib.h
#pragma once

typedef unsigned long long u64;

struct Base {
  u64 addr_ = 0;
};

struct Derived : public Base {
  int other_ = 0;
};

//--- Lib.swift
import CxxLib

public func libReadAddr(_ d: Derived) -> UInt64 {
  var dd = d
  return dd.addr_
}

//--- Main.swift
import CxxLib
import Lib

public func run(_ d: Derived) -> UInt64 {
  var dd = d
  let local = dd.addr_
  return local + libReadAddr(d)
}

// CHECK: sil {{.*}}@$e4Main3run{{.*}}
