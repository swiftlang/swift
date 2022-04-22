// async main is nested deeper in protocols than sync, use sync
// sync main is nested deeper in protocols than async, use async
// async and sync are same level, use async

// REQUIRES: concurrency

// ASYNC_NESTED: async is nested more deeply than sync main in protocol chain
// NO_ASYNC:     no async main function
// NO_SYNC:      no sync main function
// BOTH:         MainProtocol has both sync and async main
// INHERIT_SYNC: main type directly conforms to synchronous main protocol

// | async flag | has async main | has sync main | both | inherits sync | nested async | Result     | Run                                                                                                                                                       |
// |            |                |               |      |               |              | Error      | RUN: not %target-swift-frontend -disable-availability-checking -DNO_ASYNC -DNO_SYNC  -parse-as-library -typecheck -dump-ast %s 2>&1                       | %FileCheck %s --check-prefix=CHECK-IS-ERROR
// |            | x              |               |      |               |              | Async Main | RUN: %target-swift-frontend -disable-availability-checking -DNO_SYNC -parse-as-library -typecheck -dump-ast %s                                            | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// | x          |                | x             |      |               |              | Sync Main  | RUN: %target-swift-frontend -disable-availability-checking -DNO_ASYNC -async-main -parse-as-library -typecheck -dump-ast %s                               | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// | x          | x              | x             |      |               |              | Async Main | RUN: %target-swift-frontend -disable-availability-checking -async-main -parse-as-library -typecheck -dump-ast %s                                          | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// |            | x              | x             |      |               |              | Sync Main  | RUN: %target-swift-frontend -disable-availability-checking -parse-as-library -typecheck -dump-ast %s                                                      | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// |            | x              | x             |      |               | x            | Async Main | RUN: %target-swift-frontend -disable-availability-checking -DASYNC_NESTED -parse-as-library -typecheck -dump-ast %s                                       | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// |            | x              | x             |      | x             | x            | Sync Main  | RUN: %target-swift-frontend -disable-availability-checking -DINHERIT_SYNC -DASYNC_NESTED -parse-as-library -typecheck -dump-ast %s                        | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// | x          | x              | x             |      | x             | x            | Async Main | RUN: %target-swift-frontend -disable-availability-checking -DINHERIT_SYNC -DASYNC_NESTED -async-main -parse-as-library -typecheck -dump-ast %s            | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// | x          |                | x             |      | x             | x            | Sync Main  | RUN: %target-swift-frontend -disable-availability-checking -DNO_ASYNC -DINHERIT_SYNC -DASYNC_NESTED -async-main -parse-as-library -typecheck -dump-ast %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// |            |                | x             | x    |               |              | Sync Main  | RUN: %target-swift-frontend -disable-availability-checking -DBOTH -DNO_ASYNC -parse-as-library -typecheck -dump-ast %s                                    | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// | x          |                | x             | x    |               |              | Async Main | RUN: %target-swift-frontend -disable-availability-checking -DBOTH -DNO_ASYNC -async-main -parse-as-library -typecheck -dump-ast %s                        | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// tldr;
// If async flag is set, will pick an asynchronous main function if one is available and related. If none exist, will fall back on synchronous main.
// If async flag is not set, will pick a asynchronous main function if one is available and related. If none exist, will fall back on an asynchronous main
// If neither are available; error

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

// CHECK-IS-ERROR: error: 'MyMain' is annotated with @main and must provide a main static function of type {{\(\) -> Void or \(\) throws -> Void|\(\) -> Void, \(\) throws -> Void, \(\) async -> Void, or \(\) async throws -> Void}}
