// RUN: %target-typecheck-verify-swift \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -enable-experimental-feature BorrowAndMutateAccessors

// REQUIRES: objc_interop
// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_BorrowAndMutateAccessors

import Foundation

// Objective-C reaches storage through a getter, and a setter when it is
// mutable.  An accessor that is not a getter/setter -- a 'borrow' read, or a
// 'mutate' write -- has no Objective-C entry point, and storage read/written
// only through one has no getter/setter to expose, so it is not
// representable in Objective-C.  ('_read'/'_modify', 'yielding mutate', and a
// concrete copyable 'yielding borrow' all synthesize an ordinary
// getter/setter, and '@_borrowed' keeps its explicit getter, so those remain
// representable.  A 'yielding borrow' protocol requirement keeps a borrowing
// opaque read with no getter, like '@_borrowed' witness layout, so it is
// still not representable.)

@objc protocol P {
  var yieldingBorrow: Int { yielding borrow } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}
  // expected-note@-1 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  var borrowed: Int { borrow } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}
  // expected-note@-1 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  var yieldingBorrowSet: Int { yielding borrow set } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}
  // expected-note@-1 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  subscript(i: Int) -> Int { yielding borrow set } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}
  // expected-note@-1 {{inferring '@objc' because the declaration is a member of an '@objc' protocol}}

  // Ordinary requirements remain representable.
  var plain: Int { get set }
  subscript(s: String) -> Int { get }
}

@objc class C: NSObject {
  var _storage = 0

  // A concrete copyable 'yielding borrow' synthesizes an owned getter (like
  // '_read'), so with a getter present this is representable.
  @objc var yieldingBorrow: Int { yielding borrow { yield 0 } }

  @objc var borrowed: Int { borrow { return _storage } } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}

  @objc var borrowMutate: Int { borrow { return _storage } mutate { return &_storage } } // expected-error {{'borrow' and 'yielding borrow' accessors are not supported in Objective-C}}

  // Likewise for a subscript.
  @objc subscript(i: Int) -> Int { yielding borrow { yield 0 } }

  // A 'yielding mutate' write synthesizes a setter, so with a getter present
  // this is representable.
  @objc var mutable: Int { get { 0 } yielding mutate { yield &_storage } }

  // '@_borrowed' keeps an explicit getter for Objective-C and only changes the
  // Swift-side opaque accessor, so it remains allowed.
  @objc @_borrowed var borrowedAttr: Int { get { 0 } }

  // Ordinary '@objc' members are unaffected.
  @objc var plain: Int { get { 0 } }
  @objc var stored: Int = 0
}

// Not exposed to Objective-C: these accessors are fine.
protocol Q {
  var value: Int { yielding borrow }
  subscript(i: Int) -> Int { borrow }
}

class D {
  var value: Int { yielding borrow { yield 0 } }
}
