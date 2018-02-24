// RUN: %swift_driver -sdk %sdk %s | %FileCheck -check-prefix=WITHOUT-LIB %s
// RUN: %swift_driver -sdk %sdk -L %S/Inputs/ -lTestLoad %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -L %S/Inputs/ -llibTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -l%S/Inputs/libTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: cd %S && %swift_driver -sdk %sdk -lInputs/libTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s
// REQUIRES: OS=macosx
// REQUIRES: executable_test

import ObjectiveC

let ptr = objc_lookUpClass("ClassFromLibrary")
print("Loaded? \(ptr != nil)")
// WITH-LIB: Loaded? true
// WITHOUT-LIB: Loaded? false
