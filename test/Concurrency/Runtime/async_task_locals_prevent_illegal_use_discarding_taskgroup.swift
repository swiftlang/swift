// RUN: %target-fail-simple-swift( -plugin-path %swift-plugin-dir -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) 2>&1 | %FileCheck %s
//
// // TODO: could not figure out how to use 'not --crash' it never is used with target-run-simple-swift
// This test is intended to *crash*, so we're using target-fail-simple-swift
// which expects the exit code of the program to be non-zero;
// We then check stderr for the expected error message using filecheck as usual.

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

@available(SwiftStdlib 5.1, *)
enum TL {
  @TaskLocal
  static var number: Int = 2
}

// ==== ------------------------------------------------------------------------

func bindAroundGroupAddTask() async {
  await TL.$number.withValue(1111) { // ok
    await withTaskGroup(of: Int.self) { group in
      // CHECK: error: task-local: detected illegal task-local value binding at {{.*}}illegal_use_discarding_taskgroup.swift:[[# @LINE + 1]]
      TL.$number.withValue(2222) { // bad!
        print("Survived, inside withValue!") // CHECK-NOT: Survived, inside withValue!
        group.addTask {
          0 // don't actually perform the read, it would be unsafe.
        }
      }

      print("Survived the illegal call!") // CHECK-NOT: Survived the illegal call!
    }
  }
}

@main struct Main {
  static func main() async {
    await bindAroundGroupAddTask()
  }
}
