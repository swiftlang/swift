// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -I %t -serialize-diagnostics-path %t/diags.dia %t/Client.swift -verify
// RUN: c-index-test -read-diagnostics %t/diags.dia > %t/diags.deserialized_diagnostics.txt 2>&1
// RUN: %FileCheck --input-file=%t/diags.deserialized_diagnostics.txt %t/Client.swift

//--- Lib.swift

public struct S {
  public init(a: String) {}
  @discardableResult // We use @discardableResult here to give this init a distinct line number in the pretty print.
  public init(b: Int) {}
}

//--- Client.swift

import Lib

var x = S.init // expected-error {{ambiguous use of 'init'}}
// CHECK: {{.*[/\\]}}Client.swift:[[@LINE-1]]:11: error: ambiguous use of 'init'

// FIXME: Currently we can't check for 'init(a:)' because c-index-test is
// keying the source file contents on the buffer identifier, so we end up
// with duplicated 'init(b:)'.

// CHECK: Lib.S.init:3:10: note: found this candidate
// CHECK:      CONTENTS OF FILE Lib.S.init:
// CHECK:      struct S {
// CHECK-NEXT:    @discardableResult
// CHECK-NEXT:    public init(b: Int)
