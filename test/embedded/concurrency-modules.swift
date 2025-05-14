// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o -parse-as-library
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

import _Concurrency

nonisolated(unsafe) var glob: UnsafeMutableRawPointer? = nil
nonisolated(unsafe) var job: UnownedJob? = nil

public final class MyCustomExecutor: SerialExecutor, @unchecked Sendable {
    private init() {}

    nonisolated(unsafe)
    public static var shared: MyCustomExecutor = MyCustomExecutor()

    public static func installGlobalExecutor() {
        let fn: @convention(thin) () -> () = {
            MyCustomExecutor.shared.unsafeEnqueue(job!)
        }
        glob = unsafeBitCast(fn, to: UnsafeMutableRawPointer?.self)
    }

    private func enqueue(_ job: UnownedJob, withDelay nanoseconds: UInt64) {}

    private func unsafeEnqueue(_ job: UnownedJob) {
        job.runSynchronously(on: self.asUnownedSerialExecutor())
    }

    public func enqueue(_ job: consuming ExecutorJob) {
        unsafeEnqueue(UnownedJob(job))
    }

    public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
        return UnownedSerialExecutor(ordinary: self)
    }
}

// BEGIN Main.swift

import MyModule
import _Concurrency

@main
struct Entrypoint {
    static func main() async {
        MyCustomExecutor.installGlobalExecutor()
        print("OK!")
    }
}

// CHECK: OK!
