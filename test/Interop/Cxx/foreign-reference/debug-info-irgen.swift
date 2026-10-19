// RUN: %target-swift-frontend -emit-ir -g %s -I %S/Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple -o - | %FileCheck %s

// Make sure IRGen can reconstruct the debug type of an FRT declared in a namespace.

import ReferenceCounted

func use(_ x: NS.LocalCount) {}

func captureBoxed(_ x: NS.LocalCount, _ y: NS.LocalCount) -> () -> Void {
  var x = x
  let f = { use(x) }
  x = y
  return f
}

// CHECK: !DICompositeType({{.*}}identifier: "$sSo2NSO10LocalCountVz_XxD"
