// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct SomeStruct {
  func simple(_ someClosure: () -> Void) { }
}

func test(s: SomeStruct) {
  s.simple { }
  // CHECK: [[@LINE-1]]:5 | instance-method/Swift | simple(_:) | s:14swift_ide_test10SomeStructV6simpleyyyyXEF | Ref,Call,RelRec,RelCall,RelCont | rel: 2
  (((s).simple)) { }
  // CHECK: [[@LINE-1]]:9 | instance-method/Swift | simple(_:) | s:14swift_ide_test10SomeStructV6simpleyyyyXEF | Ref,Call,RelRec,RelCall,RelCont | rel: 2
}
