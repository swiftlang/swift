// Non-apple platforms don't need to worry about the version number as much
// because they can pull in the concurrency libraries with the swift
// installation.

// async main is nested deeper in protocols than sync, use sync
// sync main is nested deeper in protocols than async, use async
// async and sync are same level, use async

// REQUIRES: concurrency
// UNSUPPORTED: VENDOR=apple

// Async is deeper in the protocol chain from `MyMain`, use sync
// RUN: %target-swift-frontend -DASYNC_NESTED -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC

// Sync is deeper in the protocol chain from `MyMain`, use async
// RUN: %target-swift-frontend -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// Async and sync are the same level, use async
// RUN: %target-swift-frontend -DBOTH -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

#if ASYNC_NESTED
protocol AsyncMainProtocol { }
protocol MainProtocol : AsyncMainProtocol { }
#else
protocol MainProtocol { }
protocol AsyncMainProtocol : MainProtocol { }
#endif

#if NO_SYNC
#else
extension MainProtocol {
    static func main() { }
}
#endif

#if NO_ASYNC
#else
extension AsyncMainProtocol {
    static func main() async { }
}
#endif

#if BOTH
extension MainProtocol {
    static func main() async { }
}
#endif


#if INHERIT_SYNC
@main struct MyMain : MainProtocol {}
#else
@main struct MyMain : AsyncMainProtocol {}
#endif



// CHECK-IS-SYNC-LABEL: "MyMain" interface type='MyMain.Type'
// CHECK-IS-SYNC: (func_decl implicit "$main()" interface type='(MyMain.Type) -> () -> ()'
// CHECK-IS-SYNC:       (declref_expr implicit type='(MyMain.Type) -> () -> ()'

// CHECK-IS-ASYNC-LABEL: "MyMain" interface type='MyMain.Type'
// CHECK-IS-ASYNC: (func_decl implicit "$main()" interface type='(MyMain.Type) -> () async -> ()'
// CHECK-IS-ASYNC:       (declref_expr implicit type='(MyMain.Type) -> () async -> ()'

// CHECK-IS-ERROR: error: 'MyMain' is annotated with @main and must provide a main static function of type () -> Void or () throws -> Void

// CHECK-IS-ERROR-ASYNC: error: 'MyMain' is annotated with @main and must provide a main static function of type () -> Void, () throws -> Void, () async -> Void, or () async throws -> Void
