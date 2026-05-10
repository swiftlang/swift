// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: concurrency

@MainActor
public struct S {
  public init(_: @escaping () -> Void) { }
}

let s = S { }
// CHECK: [[@LINE-1]]:9 | struct/Swift | S | s:14swift_ide_test1SV | Ref,RelCont
// CHECK: [[@LINE-2]]:9 | constructor/Swift | init(_:) | s:14swift_ide_test1SVyACyyccfc | Ref,Call,RelCont
