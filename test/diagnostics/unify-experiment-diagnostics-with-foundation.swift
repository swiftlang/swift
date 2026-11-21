// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature UnificationDiagnostic -primary-file -verify-ignore-unrelated %s
import Foundation

// TODO: Naming the overloaded function or operator (??) would improve diagnostic
func rdar20029786(_ ns: NSString?) {
  var s2 = ns ?? "str" as String as String // expected-error {{cannot convert value of type 'String' for use in overloaded function; changing to 'NSString' reduces ambiguity}}
}
