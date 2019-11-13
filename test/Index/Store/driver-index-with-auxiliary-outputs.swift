// RUN: %empty-directory(%t)

// This test is very deliberately *not* indexing the current file; we need to
// make sure the frontend job doesn't try to emit the auxiliary outputs based
// on the non-indexed files. (This is how Xcode currently constructs -index-file
// invocations: take a normal build command and add extra arguments to it.)
// RUN: %target-build-swift -index-file -index-file-path %S/Inputs/SwiftModuleA.swift %S/Inputs/SwiftModuleA.swift %s -index-store-path %t/idx -module-name driver_index -emit-objc-header-path %t/out.h -emit-module-interface-path %t/out.swiftinterface

// RUN: test ! -f %t/out.h
// RUN: test ! -f %t/out.swiftinterface
// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

// CHECK-LABEL: module-name: driver_index
// CHECK: DEPEND START
// CHECK-NOT: Record |
// CHECK: Record | user | {{.+}}SwiftModuleA.swift
// CHECK-NOT: Record |
// CHECK: DEPEND END

#if _runtime(_ObjC)

// Do a stronger test here involving checking @objc
import ObjectiveC

public class PossiblyObjC: NSObject {
  @objc public init(x: Int) {}
}

#else // _runtime(_ObjC)

public class Boring {
  init()
}

#endif // _runtime(_ObjC)
