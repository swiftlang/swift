// REQUIRES: objc_interop

// RUN: %target-typecheck-verify-swift -enable-objc-interop -import-objc-header %S/Inputs/diag_ownership_incompatibility.h

class C {
  weak var weakVar: NoWeakClass? = nil // expected-error {{'NoWeakClass' is incompatible with 'weak' references}}
  unowned var unownedVar = NoWeakClass() // expected-error {{'NoWeakClass' is incompatible with 'unowned' references}}
}

_ = C()

weak var weakVar: NoWeakClass? = nil // expected-error {{'NoWeakClass' is incompatible with 'weak' references}}
unowned var unownedVar = NoWeakClass() // expected-error {{'NoWeakClass' is incompatible with 'unowned' references}}

// Subclasses are also incompatible.

class SwiftNoWeakClass: NoWeakClass { }

class D {
  weak var weakVar: SwiftNoWeakClass? = nil // expected-error {{'SwiftNoWeakClass' is incompatible with 'weak' references}}
  unowned var unownedVar = SwiftNoWeakClass() // expected-error {{'SwiftNoWeakClass' is incompatible with 'unowned' references}}
}

_ = D()

weak var weakSwiftVar: SwiftNoWeakClass? = nil // expected-error {{'SwiftNoWeakClass' is incompatible with 'weak' references}}
unowned var unownedSwiftVar = SwiftNoWeakClass() // expected-error {{'SwiftNoWeakClass' is incompatible with 'unowned' references}}