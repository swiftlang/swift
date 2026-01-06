// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s | %FileCheck %s
// REQUIRES: concurrency

@globalActor
actor CustomActor {
  static let shared = CustomActor()
}

// Closure attributes

func f() {
  _ = { @MainActor in }
  // CHECK:      [[@LINE-1]]:10 | class/Swift | MainActor | s:ScM | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | f() | s:14swift_ide_test1fyyF
  _ = { @CustomActor in }
  // CHECK:      [[@LINE-1]]:10 | class/Swift | CustomActor | s:14swift_ide_test11CustomActorC | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | function/Swift | f() | s:14swift_ide_test1fyyF
}

// Function type attributes

typealias MAIsolated = @MainActor (Int) -> ()
// CHECK: [[@LINE-1]]:25 | class/Swift | MainActor | s:ScM | Ref | rel: 0
// CHECK: [[@LINE-2]]:36 | struct/Swift | Int | s:Si | Ref | rel: 0
typealias CAIsolated = @CustomActor () -> Int
// CHECK: [[@LINE-1]]:25 | class/Swift | CustomActor | s:14swift_ide_test11CustomActorC | Ref | rel: 0
// CHECK: [[@LINE-2]]:43 | struct/Swift | Int | s:Si | Ref | rel: 0

// Declaration attributes

@CustomActor
// CHECK: [[@LINE-1]]:2 | class/Swift | CustomActor | s:14swift_ide_test11CustomActorC | Ref | rel: 0
class CustomIsolated {
  @CustomActor func customIsolated() {}
  // CHECK:      [[@LINE-1]]:4 | class/Swift | CustomActor | s:14swift_ide_test11CustomActorC | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | instance-method/Swift | customIsolated() | s:14swift_ide_test14CustomIsolatedC06customE0yyF
}
