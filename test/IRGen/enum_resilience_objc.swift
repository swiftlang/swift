// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -module-name enum_resilience -I %t -emit-ir -enable-resilience %s | %FileCheck %s -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -module-name enum_resilience -I %t -emit-ir -enable-resilience -O %s

// REQUIRES: objc_interop

// Because the enum is resilient we cannot pack the tag into the pointer inside of the resilient payload.
// CHECK: %T15enum_resilience9ContainerC5Multi33_{{.*}}LLO.0 = type <{ [{{(8|4)}} x i8], [1 x i8] }>

import resilient_struct

public class Container {
  private enum Multi {
    case none
    case some(Container)
    case data(ResilientRef)
  }
  private var e: Multi
  var i: Int
  init() {
    e = .none
    i = 0
  }
}
