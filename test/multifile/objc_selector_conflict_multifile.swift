// RUN: %target-typecheck-verify-swift %S/Inputs/objc_selector_conflict_multifile_other.swift
// REQUIRES: objc_interop

import Foundation

class EmitTest {}

extension EmitTest {
  @objc static func emit(_ string : String) -> String {
    // expected-error@-1 {{method 'emit' with Objective-C selector 'emit:' conflicts with previous declaration with the same Objective-C selector}}
    return ""
  }
}
