// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/OtherModule.swiftmodule %S/Inputs/definite_init_cross_module/OtherModule.swift
// RUN: %target-swift-frontend -emit-sil -verify -I %t -swift-version 5 %s > /dev/null -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/definite_init_cross_module/BridgingHeader.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/OtherModule.swiftmodule %S/Inputs/definite_init_cross_module/OtherModule.swift -enable-ownership-stripping-after-serialization
// RUN: %target-swift-frontend -emit-sil -verify -I %t -swift-version 5 %s > /dev/null -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/definite_init_cross_module/BridgingHeader.h -enable-ownership-stripping-after-serialization

import OtherModule

extension Point {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = yy // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xx: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xxx: Double, yyy: Double) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: Point) {
    // This is OK
    self = other
  }

  init(other: Point, x: Double) {
    // This is OK
    self = other
    self.x = x
  }

  init(other: Point, xx: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self = other
  }

  init(other: Point, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: Point, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = 0 // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

extension GenericPoint {
  init(xx: T, yy: T) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = yy // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xxx: T, yyy: T) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: GenericPoint<T>) {
    // This is OK
    self = other
  }

  init(other: GenericPoint<T>, x: T) {
    // This is OK
    self = other
    self.x = x
  }

  init(other: GenericPoint<T>, xx: T) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self = other
  }

  init(other: GenericPoint<T>, x: T, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: GenericPoint<T>, xx: T, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

extension GenericPoint where T == Double {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = yy // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xxx: Double, yyy: Double) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: GenericPoint<Double>) {
    // This is OK
    self = other
  }

  init(other: GenericPoint<Double>, x: Double) {
    // This is OK
    self = other
    self.x = x
  }

  init(other: GenericPoint<Double>, xx: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self = other
  }

  init(other: GenericPoint<Double>, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: GenericPoint<Double>, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = 0 // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

typealias MyGenericPoint<Q> = GenericPoint<Q>

extension MyGenericPoint {
  init(myX: T, myY: T) {
    self.x = myX // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = myY // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

extension CPoint {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}} expected-note {{use "self.init()" to initialize the struct with zero values}} {{5-5=self.init()\n}}
    self.y = yy // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xxx: Double, yyy: Double) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: CPoint) {
    // This is OK
    self = other
  }

  init(other: CPoint, x: Double) {
    // This is OK
    self = other
    self.x = x
  }

  init(other: CPoint, xx: Double) {
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}} expected-note {{use "self.init()" to initialize the struct with zero values}} {{5-5=self.init()\n}}
    self = other
  }

  init(other: CPoint, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: CPoint, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    self.y = 0 // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}


extension NonnullWrapper {
  init(p: UnsafeMutableRawPointer) {
    self.ptr = p // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
    // No suggestion for "self.init()" because this struct does not support a
    // zeroing initializer.
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}


extension PrivatePoint {
  init(xxx: Double, yyy: Double) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: PrivatePoint) {
    // This is OK
    self = other
  }

  init(other: PrivatePoint, cond: Bool) {
    if cond { self = other }
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init() {
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

extension Empty {
  init(x: Double) {
    // This is OK
    self.init()
  }

  init(other: Empty) {
    // This is okay
    self = other
  }

  init(other: Empty, cond: Bool) {
    if cond { self = other }
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xx: Double) {
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

extension GenericEmpty {
  init(x: Double) {
    // This is OK
    self.init()
  }

  init(other: GenericEmpty<T>) {
    // This is okay
    self = other
  }

  init(other: GenericEmpty<T>, cond: Bool) {
    if cond { self = other }
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}

  init(xx: Double) {
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}
