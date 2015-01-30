// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: cp %s %t/main.swift
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/ObjCWeak/ObjCWeak.m -c -o %t/ObjCWeak.o
// RUN: %target-build-swift %t/main.swift -I %S/Inputs/ObjCWeak/ -Xlinker %t/ObjCWeak.o -o %t/weak_objc_interop -Xfrontend -disable-access-control
// RUN: %target-run %t/weak_objc_interop 2>&1 | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import ObjCWeak

// Test that instances of pure Swift classes can be referenced using ObjC
// weak references.

class C {
  @objc var description: String {
    return "Swift Object"
  }
}

tryWeakReferencing { C() }
// CHECK:       before giving up strong reference:
// CHECK-NEXT:  Swift Object
// CHECK-NEXT:  after giving up strong reference:
// CHECK-NEXT:  Gone
