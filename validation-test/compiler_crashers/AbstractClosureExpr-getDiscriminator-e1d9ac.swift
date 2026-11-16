// {"kind":"emit-silgen","signature":"swift::AbstractClosureExpr::getDiscriminator() const","signatureNext":"Verifier::verifyChecked"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -disable-objc-attr-requires-foundation-module -module-name Other -o %t %t/Other.swift
// RUN: not --crash %target-swift-frontend -emit-silgen -disable-objc-attr-requires-foundation-module -I %t %t/main.swift
// REQUIRES: objc_interop
//--- Other.swift
public class Foo {
  @objc public func bar() {}
}
//--- main.swift
import Other
func g(_ fn: @autoclosure () -> Int) -> Int { fn() }
func f(_ b: Int) -> Int {
  class Local {
    @objc(bar) func bar() {}
  }
  return g(b)
}
