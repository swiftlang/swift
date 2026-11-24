// RUN: %target-swift-frontend -parse-as-library -O -module-name A -S %s -o - | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -O -module-name A -emit-ir %s -o - -Xcc -mtail-call | %FileCheck %s --check-prefix=TAIL

// REQUIRES: concurrency
// REQUIRES: CPU=wasm32

// The stub filling an eliminated async method's vtable slot passes the async
// context to swift_deletedAsyncMethodError, so the declaration has to take it.
// Wasm checks each call against the callee's type, and with tail calls an extra
// argument goes unnoticed by the validator.

public class C {
    public init() {}
    func unused() async {}
}

// CHECK-DAG: .functype _swift_dead_method_async_stub (i32, i32, i32) -> ()
// CHECK-DAG: .functype swift_deletedAsyncMethodError (i32, i32, i32) -> ()

// TAIL: declare swifttailcc void @swift_deletedAsyncMethodError(ptr{{.*}})
