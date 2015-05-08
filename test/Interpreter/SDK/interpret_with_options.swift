// RUN: %swift_driver -sdk %sdk %s | FileCheck -check-prefix=WITHOUT-LIB %s
// RUN: %swift_driver -sdk %sdk -L %S/Inputs/ -lTestLoad %s | FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -L %S/Inputs/ -llibTestLoad.dylib %s | FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -l%S/Inputs/libTestLoad.dylib %s | FileCheck -check-prefix=WITH-LIB %s
// RUN: cd %S && %swift_driver -sdk %sdk -lInputs/libTestLoad.dylib %s | FileCheck -check-prefix=WITH-LIB %s
// REQUIRES: OS=macosx

import ObjectiveC

// FIXME: <rdar://problem/19302805> Crash when NSClassFromString returns nil
@asmname("objc_lookUpClass")
func lookUpClassOpaque(name: UnsafePointer<CChar>) -> COpaquePointer

let ptr = lookUpClassOpaque("ClassFromLibrary")
print("Loaded? \(ptr != nil)")
// WITH-LIB: Loaded? true
// WITHOUT-LIB: Loaded? false
