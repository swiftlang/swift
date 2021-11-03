// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -sdk %clang-importer-sdk -I %t
// REQUIRES: objc_interop

import Foundation

class InvalidOpenExtensionClass { }

open extension InvalidOpenExtensionClass {  // expected-error {{extensions cannot be declared 'open'; declare individual members as 'open' instead}} {{1-6=}} {{3-3=public }} {{9-9=open }} {{9-9=open }} {{3-3=public }}
  private func A() { } // OK
  @objc func B() { } // Insert open 
  func C() { } // Insert public
  @objc private var D: Int { 3 } // Okay
  @objc var E: Int { 3 } // Insert open
  var F: Int { 3 } // Insert public
  private var G: Int { 3 } // Okay
}

@objc
@objcMembers
class InvalidOpenExtensionObjcClass: NSObject { }

open extension InvalidOpenExtensionObjcClass {  // expected-error {{extensions cannot be declared 'open'; declare individual members as 'open' instead}} {{1-6=}} {{3-3=open }} {{9-9=open }} {{9-9=open }} {{3-3=open }}
  private func A() { } // OK
  @objc func B() { } // Insert open 
  func C() { } // Insert open
  @objc private var D: Int { 3 } // Okay
  @objc var E: Int { 3 } // Insert open
  var F: Int { 3 } // Insert open
  private var G: Int { 3 } // Okay
}