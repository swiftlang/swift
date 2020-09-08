// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify -verify-ignore-unknown %s -swift-version 5 -enable-source-import -I %S/Inputs -enable-experimental-concurrency
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module -swift-version 5 -enable-source-import -I %S/Inputs -enable-experimental-concurrency | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -dump-ast -disable-objc-attr-requires-foundation-module %s -swift-version 5 -enable-source-import -I %S/Inputs -enable-experimental-concurrency > %t.ast
// RUN: %FileCheck -check-prefix CHECK-DUMP %s < %t.ast
// REQUIRES: objc_interop
import Foundation

// CHECK: class Concurrency
class Concurrency {
  // CHECK: @objc func doBigJob() async -> Int
  // CHECK-DUMP: func_decl{{.*}}doBigJob{{.*}}foreign_async=@convention(block) (Int) -> (),completion_handler_param=0
  @objc func doBigJob() async -> Int { return 0 }

  // CHECK: @objc func doBigJobOrFail(_: Int) async throws -> (AnyObject, Int)
  // CHECK-DUMP: func_decl{{.*}}doBigJobOrFail{{.*}}foreign_async=@convention(block) (Optional<AnyObject>, Int, Optional<Error>) -> (),completion_handler_param=1,error_param=2
  @objc func doBigJobOrFail(_: Int) async throws -> (AnyObject, Int) { return (self, 0) }

  @objc func takeAnAsync(_ fn: () async -> Int) { } // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-1{{'async' function types cannot be represented in Objective-C}}

  @objc class func createAsynchronously() async -> Self? { nil }
  // expected-error@-1{{asynchronous method returning 'Self' cannot be '@objc'}}
}
