// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck -verify -serialize-diagnostics-path %t/serialized.dia %s 2>&1 | %FileCheck %s --check-prefixes CHECK,CHECK-WARNINGS-AS-WARNINGS --implicit-check-not error: --implicit-check-not note: --implicit-check-not warning:
// RUN: not %target-swift-frontend -typecheck -verify -warnings-as-errors %s 2>&1 | %FileCheck %s -check-prefixes CHECK,CHECK-WARNINGS-AS-ERRORS --implicit-check-not error: --implicit-check-not note: --implicit-check-not warning:
// RUN: %FileCheck %s -check-prefix CHECK-SERIALIZED <%t/serialized.dia

// Wrong message
let x: Int = "hello, world!" // expected-error {{foo bar baz}}
// CHECK-NOT: error: cannot convert value of type 'String' to specified type 'Int'
// CHECK: [[@LINE-2]]:50: error: incorrect message found

// Wrong column
let y: Int = "hello, world!" // expected-error@:49 {{cannot convert value of type}}
// CHECK: error: message found at column 14 but was expected to appear at column 49

// Wrong fix-it
let z: Int = "hello, world!" as Any
// expected-error@-1 {{cannot convert value of type}} {{3-3=foobarbaz}}
// CHECK: error: expected fix-it not seen; actual fix-it seen: {{[{][{]}}36-36= as! Int{{[}][}]}}

// Expected no fix-it
let a: Bool = "hello, world!" as Any
// expected-error@-1 {{cannot convert value of type}} {{none}}
// CHECK: error: expected no fix-its; actual fix-it seen: {{[{][{]}}37-37= as! Bool{{[}][}]}}

// Unexpected error
_ = foo()
// CHECK: error: unexpected error produced: cannot find 'foo' in scope

func b() {
  let c = 2
}
// CHECK-WARNINGS-AS-WARNINGS: error: unexpected warning produced: initialization of immutable value 'c' was never used
// CHECK-WARNINGS-AS-ERRORS: error: unexpected error produced: initialization of immutable value 'c' was never used

typealias Crap = () -> ()
extension Crap {} // expected-error {{non-nominal type 'Crap' (aka '() -> ()') cannot be extended}} {{documentation-file=foo-bar-baz}}
// CHECK: error: expected documentation file not seen; actual documentation file: {{[{][{]}}documentation-file=nominal-types{{[}][}]}}

extension Crap {} // expected-error {{non-nominal type 'Crap' (aka '() -> ()') cannot be extended}} {{documentation-file=nominal-types}} {{documentation-file=nominal-types}}
// CHECK: error: each verified diagnostic may only have one {{[{][{]}}documentation-file=<#notes#>{{[}][}]}} declaration

extension Crap {} // expected-error {{non-nominal type 'Crap' (aka '() -> ()') cannot be extended}} {{documentation-file=nominal-types,foo-bar-baz}}
// CHECK: error: expected documentation file not seen; actual documentation file: {{[{][{]}}documentation-file=nominal-types{{[}][}]}}

// CHECK: error: unexpected note produced: 'Bool' declared here
// CHECK: note: file 'Swift.Bool' is not parsed for 'expected' statements. Use '-verify-additional-file Swift.Bool' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file

// Verify the serialized diags have the right magic at the top.
// CHECK-SERIALIZED: DIA
