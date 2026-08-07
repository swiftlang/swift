// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s --dump-input=always
// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)  | %FileCheck %s --dump-input=always
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// REQUIRES: concurrency_runtime

// rdar://103606995
// UNSUPPORTED: back_deployment_runtime

// RUN: %if embedded_dispatch_executor %{ %empty-directory(%t.embedded) %}
// RUN: %if embedded_dispatch_executor %{ %target-swift-frontend -target %embedded-dispatch-target-triple -enable-experimental-feature Embedded -disable-availability-checking -parse-as-library -wmo %s -c -o %t.embedded/a.o %}
// RUN: %if embedded_dispatch_executor %{ %target-clang -target %embedded-dispatch-target-triple %target-clang-resource-dir-opt %t.embedded/a.o -o %t.embedded/a.out %embedded-dispatch-concurrency-libraries %target-swift-dead-strip-opt %}
// RUN: %if embedded_dispatch_executor %{ %target-run %t.embedded/a.out | %FileCheck %s %}

import _Concurrency
#if !$Embedded
import Dispatch
#endif

@available(SwiftStdlib 5.1, *)
func test_detach_cancel_taskGroup() async {
  print(#function) // CHECK: test_detach_cancel_taskGroup

  await withTaskGroup(of: Void.self) { group in
    group.cancelAll() // immediately cancel the group
    print("group.cancel()") // CHECK: group.cancel()

    group.addTask {
      // immediately cancelled child task...
      await withTaskCancellationHandler {
        print("child: operation, was cancelled: \(Task.isCancelled)")
      } onCancel: {
        print("child: onCancel, was cancelled: \(Task.isCancelled)")
      }
    }
    // CHECK: child: onCancel, was cancelled: true
    // CHECK: child: operation, was cancelled: true
  }

  print("done") // CHECK: done
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  static func main() async {
    await test_detach_cancel_taskGroup()
  }
}
