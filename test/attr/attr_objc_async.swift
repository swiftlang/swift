// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify -verify-ignore-unknown %s -swift-version 4 -enable-source-import -I %S/Inputs -enable-swift3-objc-inference -enable-experimental-concurrency
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-source-import -I %S/Inputs -enable-swift3-objc-inference -enable-experimental-concurrency | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -dump-ast -disable-objc-attr-requires-foundation-module %s -swift-version 4 -enable-source-import -I %S/Inputs -enable-swift3-objc-inference -enable-experimental-concurrency > %t.ast
// RUN: %FileCheck -check-prefix CHECK-DUMP %s < %t.ast
// REQUIRES: objc_interop

// async cannot be compiled with @objc.
// CHECK-DUMP: class Concurrency
class Concurrency {
  @objc func doBigJob() async -> Int { return 0 } // expected-error{{'async' function cannot be represented in Objective-C}}

  @objc func takeAnAsync(_ fn: () async -> Int) { } // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}}
  // expected-note@-1{{'async' function types cannot be represented in Objective-C}}
}
