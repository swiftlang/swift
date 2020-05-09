// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/OtherModule.swiftmodule %S/Inputs/definite_init_cross_module/OtherModule.swift
// RUN: %target-swift-frontend -emit-sil -verify -verify-ignore-unknown -I %t -swift-version 4 %s > /dev/null -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/definite_init_cross_module/BridgingHeader.h

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-ownership-stripping-after-serialization -emit-module -emit-module-path=%t/OtherModule.swiftmodule %S/Inputs/definite_init_cross_module/OtherModule.swift
// RUN: %target-swift-frontend -enable-ownership-stripping-after-serialization -emit-sil -verify -verify-ignore-unknown -I %t -swift-version 4 %s > /dev/null -enable-objc-interop -disable-objc-attr-requires-foundation-module -import-objc-header %S/Inputs/definite_init_cross_module/BridgingHeader.h

import OtherModule

extension Point {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-warning {{initializer for struct 'Point' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self.y = yy
  }

  init(xx: Double) {
    self.x = xx // expected-warning {{initializer for struct 'Point' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
  } // expected-error {{return from initializer without initializing all stored properties}}

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
    self.x = xx // expected-warning {{initializer for struct 'Point' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self = other
  }

  init(other: Point, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: Point, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-warning {{initializer for struct 'Point' must use "self.init(...)" or "self = ..." on all paths because it is not in module 'OtherModule'}}
    self.y = 0
  }
}

extension ImmutablePoint {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-warning {{initializer for struct 'ImmutablePoint' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self.y = yy
  }

  init(xxx: Double, yyy: Double) {
    // This is OK
    self.init(x: xxx, y: yyy)
  }

  init(other: ImmutablePoint) {
    // This is OK
    self = other
  }

  init(other: ImmutablePoint, x: Double) {
    self = other
    self.x = x // expected-error {{immutable value 'self.x' may only be initialized once}}
  }

  init(other: ImmutablePoint, xx: Double) {
    self.x = xx // expected-warning {{initializer for struct 'ImmutablePoint' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self = other // expected-error {{immutable value 'self.x' may only be initialized once}}
  }

  init(other: ImmutablePoint, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x } // expected-error {{immutable value 'self.x' may only be initialized once}}
  }

  init(other: ImmutablePoint, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-error {{immutable value 'self.x' may only be initialized once}}
    self.y = 0 // expected-error {{immutable value 'self.y' may only be initialized once}}
  }
}

extension GenericPoint {
  init(xx: T, yy: T) {
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<T>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self.y = yy
  }

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
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<T>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self = other
  }

  init(other: GenericPoint<T>, x: T, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: GenericPoint<T>, xx: T, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<T>' must use "self.init(...)" or "self = ..." on all paths because it is not in module 'OtherModule'}}
    self.y = xx
  }
}

extension GenericPoint where T == Double {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<Double>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self.y = yy
  }

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
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<Double>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self = other
  }

  init(other: GenericPoint<Double>, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: GenericPoint<Double>, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-warning {{initializer for struct 'GenericPoint<Double>' must use "self.init(...)" or "self = ..." on all paths because it is not in module 'OtherModule'}}
    self.y = 0
  }
}

typealias MyGenericPoint<Q> = GenericPoint<Q>

extension MyGenericPoint {
  // FIXME: Should preserve type sugar.
  init(myX: T, myY: T) {
    self.x = myX // expected-warning {{initializer for struct 'GenericPoint<T>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
    self.y = myY
  }
}

extension CPoint {
  init(xx: Double, yy: Double) {
    self.x = xx // expected-warning {{initializer for struct 'CPoint' must use "self.init(...)" or "self = ..." because the struct was imported from C}} expected-note {{use "self.init()" to initialize the struct with zero values}} {{5-5=self.init()\n}}
    self.y = yy
  }

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
    self.x = xx // expected-warning {{initializer for struct 'CPoint' must use "self.init(...)" or "self = ..." because the struct was imported from C}} expected-note {{use "self.init()" to initialize the struct with zero values}} {{5-5=self.init()\n}}
    self = other
  }

  init(other: CPoint, x: Double, cond: Bool) {
    // This is OK
    self = other
    if cond { self.x = x }
  }

  init(other: CPoint, xx: Double, cond: Bool) {
    if cond { self = other }
    self.x = xx // expected-warning {{initializer for struct 'CPoint' must use "self.init(...)" or "self = ..." on all paths because the struct was imported from C}}
    self.y = 0
  }
}


extension NonnullWrapper {
  init(p: UnsafeMutableRawPointer) {
    self.ptr = p // expected-warning {{initializer for struct 'NonnullWrapper' must use "self.init(...)" or "self = ..." because the struct was imported from C}}
    // No suggestion for "self.init()" because this struct does not support a
    // zeroing initializer.
  }
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
  } // expected-error {{return from initializer without initializing all stored properties}}

  // Ideally we wouldn't mention the names of non-public stored properties
  // across module boundaries, but this will go away in Swift 5 mode anyway.
  init() {
  } // expected-error {{return from initializer without initializing all stored properties}}
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
  } // expected-warning {{initializer for struct 'Empty' must use "self.init(...)" or "self = ..." on all paths because it is not in module 'OtherModule'}}

  init(xx: Double) {
  } // expected-warning {{initializer for struct 'Empty' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
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
  } // expected-warning {{initializer for struct 'GenericEmpty<T>' must use "self.init(...)" or "self = ..." on all paths because it is not in module 'OtherModule'}}

  init(xx: Double) {
  } // expected-warning {{initializer for struct 'GenericEmpty<T>' must use "self.init(...)" or "self = ..." because it is not in module 'OtherModule'}}
}
