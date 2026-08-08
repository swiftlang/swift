// Casting a class to a single class-constrained protocol makes IRGen look up the
// class vtable to find the conformance index, which deserializes the vtable from
// the imported module. That deserialization reads function declarations after the
// module has committed to SILStage::Lowered, so it must not seed a per-function
// stage of Canonical onto them.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -emit-module -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -emit-ir -I %t -module-name Client %t/Client.swift | %FileCheck %s

//--- Lib.swift

public protocol P {}

open class C: P {
  public init() {}
  open func f() {}
}

//--- Client.swift

import Lib

public protocol Q: AnyObject {}

// CHECK-LABEL: define {{.*}}@"$s6Client4cast{{.*}}"
public func cast(_ c: C) -> Q? {
  return c as? Q
}
