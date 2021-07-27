// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library %import-libdispatch) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Dispatch

// For sleep
#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@available(SwiftStdlib 5.5, *)
enum TL {
  @TaskLocal
  static var number: Int = 0
  @TaskLocal
  static var other: Int = 0
}

@available(SwiftStdlib 5.5, *)
@discardableResult
func printTaskLocal<V>(
    _ key: TaskLocal<V>,
    _ expected: V? = nil,
    file: String = #file, line: UInt = #line
) -> V? {
  let value = key.get()
  print("\(key) (\(value)) at \(file):\(line)")
  if let expected = expected {
    assert("\(expected)" == "\(value)",
        "Expected [\(expected)] but found: \(value), at \(file):\(line)")
  }
  return expected
}

// ==== ------------------------------------------------------------------------

@available(SwiftStdlib 5.5, *)
func copyTo_sync_noWait() {
  print(#function)
  TL.$number.withValue(1111) {
    TL.$number.withValue(2222) {
      TL.$other.withValue(9999) {
        Task {
          printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (2222)
          printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          TL.$number.withValue(3333) {
            printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (3333)
            printTaskLocal(TL.$other) // CHECK: TaskLocal<Int>(defaultValue: 0) (9999)
          }
        }
      }
    }
  }

  sleep(1)
}

@available(SwiftStdlib 5.5, *)
func copyTo_sync_noValues() {
  Task {
    printTaskLocal(TL.$number) // CHECK: TaskLocal<Int>(defaultValue: 0) (0)
  }

  sleep(1)
}

/// Similar to tests in `async_task_locals_copy_to_async_ but without any task involved at the top level.
@available(SwiftStdlib 5.5, *)
@main struct Main {
  static func main() {
    copyTo_sync_noWait()
    copyTo_sync_noValues()
  }
}
