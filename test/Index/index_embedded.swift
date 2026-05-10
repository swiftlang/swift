// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -enable-experimental-feature Embedded -target %target-cpu-apple-macos14 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

struct SomeStruct {
  func simple(_ someClosure: () -> Void) { }
}

func test(s: SomeStruct) {
  s.simple { }
  // CHECK: [[@LINE-1]]:5 | instance-method/Swift | simple(_:) | s:14swift_ide_test10SomeStructV6simpleyyyyXEF | Ref,Call,RelCall,RelCont | rel: 1
}
