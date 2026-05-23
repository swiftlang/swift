// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.2-abi-triple %s | %FileCheck --check-prefix=CHECK-6_2 %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.1-abi-triple %s | %FileCheck --check-prefix=CHECK-6_1 %s

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=xros
// REQUIRES: concurrency

@available(SwiftStdlib 6.2, *)
nonisolated(nonsending)
public func testTaskPriorityEscalationHandler() async -> Int {
  return await withTaskPriorityEscalationHandler {
    return 42
  } onPriorityEscalated: { _, _ in
  }
}

// CHECK-6_2: declare swiftcc ptr @swift_task_addPriorityEscalationHandler
// CHECK-6_2: declare swiftcc void @swift_task_removePriorityEscalationHandler

// CHECK-6_1: declare extern_weak swiftcc ptr @swift_task_addPriorityEscalationHandler
// CHECK-6_1: declare extern_weak swiftcc void @swift_task_removePriorityEscalationHandler
