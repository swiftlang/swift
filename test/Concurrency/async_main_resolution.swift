// This test aims to show that no preference is given to either the async or
// sync main function. The most specific, valid, main function will be
// selected if one exists. If two main functions could exist, the usage is
// ambiguous.

// async main is nested deeper in protocols than sync, use sync
// sync main is nested deeper in protocols than async, use async
// async and sync are same level, error

// REQUIRES: concurrency

// ASYNC_NESTED: async is nested more deeply than sync main in protocol chain
// NO_ASYNC:     no async main function
// NO_SYNC:      no sync main function
// BOTH:         MainProtocol has both sync and async main
// INHERIT_SYNC: main type directly conforms to synchronous main protocol

// | has async main | has sync main | both | inherits sync | nested async | Result |                         | Run                                                                                                                                          |
// |                |               |      |               |              | Error  | No main                 | RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -DNO_SYNC -DNO_ASYNC -parse-as-library -typecheck -dump-ast %s 2>&1           | %FileCheck %s --check-prefix=CHECK-IS-ERROR1
// | x              | x             | x    | x             |              | Error  | Ambiguous main in MainP | RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -DBOTH -DINHERIT_SYNC -parse-as-library -typecheck -dump-ast %s 2>&1          | %FileCheck %s --check-prefix=CHECK-IS-ERROR2
// |                | x             | x    | x             |              | Error  | Ambiguous main in MainP | RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -DBOTH -DINHERIT_SYNC -parse-as-library -typecheck -dump-ast %s 2>&1          | %FileCheck %s --check-prefix=CHECK-IS-ERROR2
// | x              | x             | x    |               |              | Async  | Directly selected       | RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -DBOTH -parse-as-library -typecheck -dump-ast %s                                  | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// | x              | x             |      |               |              | Async  | Directly selected       | RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -typecheck -dump-ast %s                                         | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// |                | x             |      |               |              | Sync   | Indirectly selected     | RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -DNO_ASYNC -parse-as-library -typecheck -dump-ast %s                              | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// | x              | x             |      | x             | x            | Sync   | Directly selected       | RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -DINHERIT_SYNC -DASYNC_NESTED -parse-as-library -typecheck -dump-ast %s           | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// | x              |               |      | x             | x            | Async  | Indirectly selected     | RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -DNO_SYNC -DINHERIT_SYNC -DASYNC_NESTED -parse-as-library -typecheck -dump-ast %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC
// | x              |               |      | x             |              | Error  | Unrelated async main    | RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -DNO_SYNC -DINHERIT_SYNC -parse-as-library -typecheck -dump-ast %s 2>&1       | %FileCheck %s --check-prefix=CHECK-IS-ERROR1
// |                | x             |      |               | x            | Error  | Unrelated sync main     | RUN: not %target-swift-frontend -target %target-swift-5.1-abi-triple -DNO_ASYNC -DASYNC_NESTED -parse-as-library -typecheck -dump-ast %s  2>&1     | %FileCheck %s --check-prefix=CHECK-IS-ERROR1

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

// CHECK-IS-SYNC-LABEL: "MyMain" interface_type="MyMain.Type"
// CHECK-IS-SYNC: (func_decl {{.*}}implicit range={{.*}} "$main()" interface_type="(MyMain.Type) -> () -> ()"
// CHECK-IS-SYNC:       (declref_expr implicit type="(MyMain.Type) -> () -> ()"

// CHECK-IS-ASYNC-LABEL: "MyMain" interface_type="MyMain.Type"
// CHECK-IS-ASYNC: (func_decl {{.*}}implicit range={{.*}} "$main()" interface_type="(MyMain.Type) -> () async -> ()"
// CHECK-IS-ASYNC:       (declref_expr implicit type="(MyMain.Type) -> () async -> ()"

// CHECK-IS-ERROR1: error: 'MyMain' is annotated with '@main' and must provide a main static function of type {{\(\) -> Void or \(\) throws -> Void|\(\) -> Void, \(\) throws -> Void, \(\) async -> Void, or \(\) async throws -> Void}}
// CHECK-IS-ERROR2: error: ambiguous use of 'main'
