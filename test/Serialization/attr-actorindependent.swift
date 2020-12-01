// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-concurrency -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck -check-prefix BC-CHECK %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck -check-prefix MODULE-CHECK %s
// RUN: %target-swift-frontend -enable-experimental-concurrency -emit-module-path %t/b.swiftmodule -module-name a  %t/a.swiftmodule
// RUN: cmp -s %t/a.swiftmodule %t/b.swiftmodule

// REQUIRES: concurrency

///////////
// This test checks for correct serialization & deserialization of
// @actorIndependent and @actorIndependent(unsafe)

// look for correct annotation after first deserialization's module print:

// MODULE-CHECK:      actor class UnsafeCounter {
// MODULE-CHECK-NEXT:   @actorIndependent(unsafe) var storage: Int
// MODULE-CHECK-NEXT:   @actorIndependent var count: Int
// MODULE-CHECK-NEXT:   var actorCount: Int
// MODULE-CHECK-NEXT:   @actorIndependent(unsafe) func enqueue(partialTask: PartialAsyncTask)
// MODULE-CHECK-NEXT:   init()
// MODULE-CHECK-NEXT: }

// and look for unsafe and safe versions of decl in BC dump:

// BC-CHECK-NOT: UnknownCode
// BC-CHECK: <ActorIndependent_DECL_ATTR abbrevid={{[0-9]+}} op0=1/>
// BC-CHECK: <ActorIndependent_DECL_ATTR abbrevid={{[0-9]+}} op0=0/>


actor class UnsafeCounter {

  @actorIndependent(unsafe)
  private var storage : Int = 0

  @actorIndependent
  var count : Int {
    get { storage }
    set { storage = newValue }
  }

  var actorCount : Int {
    get { storage }
    set { storage = newValue }
  }
}
