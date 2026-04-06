// RUN: %target-run-simple-swift(-parse-as-library) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

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
