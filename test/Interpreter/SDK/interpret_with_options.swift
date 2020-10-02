// RUN: %empty-directory(%t)
// RUN: echo "@interface ClassFromLibrary @end; @implementation ClassFromLibrary @end" | %clang -isysroot %sdk -Wno-objc-root-class -shared -o %t/libTestLoad.dylib -lobjc -x objective-c -
// RUN: %swift_driver -sdk %sdk %s | %FileCheck -check-prefix=WITHOUT-LIB %s
// RUN: %swift_driver -sdk %sdk -L %t -lTestLoad %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -L %t -llibTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: %swift_driver -sdk %sdk -l%t/libTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s
// RUN: cd %S && %swift_driver -sdk %sdk -l%t/libTestLoad.dylib %s | %FileCheck -check-prefix=WITH-LIB %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// REQUIRES: swift_interpreter

import ObjectiveC

// FIXME: <rdar://problem/19302805> Crash when NSClassFromString returns nil
@_silgen_name("objc_lookUpClass")
func lookUpClassOpaque(_ name: UnsafePointer<CChar>) -> OpaquePointer?

let ptr = lookUpClassOpaque("ClassFromLibrary")
print("Loaded? \(ptr != nil)")
// WITH-LIB: Loaded? true
// WITHOUT-LIB: Loaded? false
