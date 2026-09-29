// RUN: split-file %s %t
//
// RUN: %target-swift-emit-sil -cxx-interoperability-mode=default \
// RUN:   -I %t/Inputs %t/main.swift | %FileCheck %s
//
// REQUIRES: objc_interop

// Verify that passing a CF-bridged pointer type through a C++ template
// with `const T &` does not crash with a TYPE MISMATCH assertion due to
// optionality differences.

//--- Inputs/module.modulemap
module ObjCxxHeader {
  header "objcxx-header.h"
  requires cplusplus, objc
}

//--- Inputs/objcxx-header.h
#pragma once

struct __attribute__((objc_bridge(id))) __MyCFType { int etc; };
typedef struct __MyCFType *MyCFTypeRef;

template <typename T> void templatedFunc(T) {}

struct TemplatedCtor {
    template <typename T> TemplatedCtor(T const &) {}
};

struct TemplatedXCtor {
    template <typename T> explicit TemplatedXCtor(T const &) {}
};

//--- main.swift
import ObjCxxHeader

// CHECK-LABEL: sil @$s4main4test3objySo11MyCFTypeRefa_tF
// CHECK-NOT: init_enum_data_addr {{.*}} Optional
// CHECK: } // end sil function
public func test(obj: MyCFType) {
    templatedFunc(obj)
    let _ = TemplatedCtor(obj)
    let _ = TemplatedXCtor(obj)
}
