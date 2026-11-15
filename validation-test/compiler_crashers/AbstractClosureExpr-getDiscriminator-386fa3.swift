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
func f(_ x: Bool, _ y: Bool) -> Bool {
  class Local {
    @objc(bar) func bar() {}
  }
  return x || y
}
