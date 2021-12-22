// async main is nested deeper in protocols than sync, should use sync (always)
// sync main is nested deeper in protocols than async, use async if supported
// async and sync are same level, use async if supported

// async main is nested in the protocol chain from `MyMain`
// Always choose Sync overload
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -DASYNC_NESTED -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.0 -DASYNC_NESTED -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC

// sync main is deeper in the protocol chain from `MyMain`
// Choose async when available
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.0 -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// sync and async main are at same level (In MainProtocol) to `MyMain`.
// Choose async when available
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -DBOTH -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -DBOTH -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// async main is the only option on the protocol chain
// Choose async if we support it, error otherwise
// RUN: not %target-swift-frontend -target x86_64-apple-macosx10.9 -DASYNC_NESTED -typecheck -dump-ast -parse-as-library %s 2>&1 | %FileCheck %s --check-prefix=CHECK-IS-ERROR
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -DASYNC_NESTED -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// sync main is the only option on the protocol chain
// Always choose sync
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -DINHERIT_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC

// No synchronous, choose async if we support it, error otherwise
// RUN: not %target-swift-frontend -target x86_64-apple-macosx10.9 -DNO_SYNC -typecheck -dump-ast -parse-as-library %s 2>&1 | %FileCheck %s --check-prefix=CHECK-IS-ERROR
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -DNO_SYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-ASYNC

// No asynchronous, choose sync
// RUN: %target-swift-frontend -target x86_64-apple-macosx10.9 -DNO_ASYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC
// RUN: %target-swift-frontend -target x86_64-apple-macosx11.9 -DNO_ASYNC -typecheck -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-IS-SYNC

// No main functions
// RUN: not %target-swift-frontend -target x86_64-apple-macosx10.9 -DNO_SYNC -DNO_ASYNC -typecheck -dump-ast -parse-as-library %s 2>&1 | %FileCheck %s --check-prefix=CHECK-IS-ERROR
// RUN: not %target-swift-frontend -target x86_64-apple-macosx11.9 -DNO_SYNC -DNO_ASYNC -typecheck -dump-ast -parse-as-library %s 2>&1 | %FileCheck %s --check-prefix=CHECK-IS-ERROR-ASYNC

// REQUIRES: concurrency
// REQUIRES: OS=macosx

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
    @available(macOS 10.15, *)
    static func main() async { }
}
#endif

#if BOTH
extension MainProtocol {
    @available(macOS 10.15, *)
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
