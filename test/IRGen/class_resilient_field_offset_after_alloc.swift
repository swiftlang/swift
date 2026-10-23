// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -parse-as-library -module-name test -I %t -emit-ir -O %s | %FileCheck %s

// Check that a load of a class field offset global, which is only initialized
// by the class metadata accessor, is not moved above the allocation of the
// class instance (rdar://190156562).

import resilient_struct

public struct Payload {
  public let object: AnyObject
  public let size: Size?
}

final class Box {
  let payload: Payload
  let key: Int
  var count = 0

  init(payload: Payload, key: Int) {
    self.payload = payload
    self.key = key
  }
}

public final class Store {
  private var boxes: [Int: Box] = [:]

  public init() {}

  // CHECK-LABEL: define {{.*}} @"$s4test5StoreC3box3for7payloadyXlSi_AA7PayloadVtF"
  // CHECK:         call swiftcc %swift.metadata_response @"$s4test3BoxCMa"
  // CHECK:         call noalias ptr @swift_allocObject
  // CHECK-NOT:     ret
  // CHECK:         load {{i32|i64}}, ptr @"$s4test3BoxC7payloadAA7PayloadVvpWvd"
  // CHECK:       }
  @inline(never)
  public func box(for key: Int, payload: Payload) -> AnyObject {
    if let box = boxes[key], box.payload.object === payload.object {
      return box
    }
    let box = Box(payload: payload, key: key)
    boxes[key] = box
    return box
  }
}
