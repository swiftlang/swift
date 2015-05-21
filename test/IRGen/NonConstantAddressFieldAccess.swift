// RUN: %target-build-swift %S/Inputs/ObjcSuperClass.swift %s -module-name a -emit-ir 2>&1 | FileCheck %s

// REQUIRES: objc_interop

// CHECK: @_TWvdvC1a12ObjCSubclass5fieldSi = global i64 16
// CHECK: @_TWvdvC1a12ObjCSubclass5fieldSi = external global i64

func test(o: ObjCSubclass) {
  o.field = 10
}
