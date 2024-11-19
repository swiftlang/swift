// RUN: %target-run-simple-swift(-target %target-future-triple -parse-stdlib -parse-as-library) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// Compiler crashes because builtin "ifdef_SWIFT_STDLIB_PRINT_DISABLED"() gets lowered as "i32 0",
// which triggers assertion in LLVM, which expects it to be i1
// XFAIL: freestanding

import Swift
import _Concurrency

#if canImport(Darwin)
import Darwin
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func equalThreadIDs(_ a: ThreadID, _ b: ThreadID) -> Bool { pthread_equal(a, b) != 0 }
#elseif canImport(Glibc)
import Glibc
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func equalThreadIDs(_ a: ThreadID, _ b: ThreadID) -> Bool { pthread_equal(a, b) != 0 }
#elseif os(Windows)
import WinSDK
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { GetCurrentThreadId() }
func equalThreadIDs(_ a: ThreadID, _ b: ThreadID) -> Bool { a == b }
#elseif os(WASI)
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { 0 }
func equalThreadIDs(_ a: ThreadID, _ b: ThreadID) -> Bool { a == b }
#endif

var mainThread: ThreadID?
func isMainThread() -> Bool {
    return equalThreadIDs(getCurrentThreadID(), mainThread!)
}

@_silgen_name("swift_task_isCurrentExecutor")
private func isCurrentExecutor(_ executor: Builtin.Executor) -> Bool

func getExecutor(_ a: any Actor) -> Builtin.Executor {
  let pack: (AnyObject, UnsafeRawPointer?) = (a, UnsafeRawPointer?.none)
  return unsafeBitCast(pack, to: Builtin.Executor.self)
}

func isCurrent(_ a: any Actor) -> Bool {
  return isCurrentExecutor(getExecutor(a))
}

actor Foo {
    let name: String
    let child: Foo?

    init(_ name: String, _ child: Foo?) {
        self.name = name
        self.child = child
    }

    isolated deinit {
      print("DEINIT: \(self.name) isolated:\(isCurrent(self)) mainThread:\(isMainThread())")
    }
}

// CHECK: DEINIT: a isolated:true mainThread:true
// CHECK: DEINIT: b isolated:true mainThread:true
// CHECK: DEINIT: c isolated:true mainThread:true
// CHECK: DEINIT: d isolated:true mainThread:true
// CHECK: DONE

@main
struct Main {
    static func main() async {
        mainThread = getCurrentThreadID()
        do {
            _ = Foo("a", Foo("b", Foo("c", Foo("d", nil))))
        }
        print("DONE")
    }
}
