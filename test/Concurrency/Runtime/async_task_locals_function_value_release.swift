// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// RUN: %if embedded_cooperative_executor %{ %target-run-embedded-cooperative-swift(-plugin-path %swift-plugin-dir) | %FileCheck %s %}
// RUN: %if embedded_dispatch_executor %{ %target-run-embedded-dispatch-swift(-plugin-path %swift-plugin-dir) | %FileCheck %s %}

import _Concurrency

// In optimized builds specifically the task local add builtins (AddTaskLocalValue/TaskLocalValuePush)
// would end up using the wrong type in this example, and attempt to demangle
// at runtime a type including ~Copyable -- which cannot be represented at runtime
// and therefore crash while trying to do so. 

@TaskLocal var taskLocal: (() -> Void)?

@main
struct Main {
  static func main() {
    $taskLocal.withValue({}) {
      print("inside withValue") // CHECK: inside withValue
    }
    print("done") // CHECK: done
  }
}
