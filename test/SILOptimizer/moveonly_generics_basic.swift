// RUN: %target-swift-frontend %s -sil-verify-all -verify -emit-sil -enable-experimental-feature MoveOnlyPartialReinitialization

// REQUIRES: swift_feature_MoveOnlyPartialReinitialization

/// MARK: types

struct FileDescriptor: ~Copyable { let fd: Int = 0 }

struct Wrap<T: ~Copyable>: ~Copyable {
  var item: T
  init(_ t: consuming T) { self.item = t }
}
extension Wrap: Copyable where T: Copyable {}

/// MARK: utilities

func borrowAny<T: ~Copyable>(_ t: borrowing T) {}

/// MARK: tests

func barebones() {
  let nc = FileDescriptor() // expected-error {{'nc' consumed more than once}}
  borrowAny(nc)
  let _ = nc // expected-note {{consumed}}
  let _ = nc // expected-note {{consumed}}

  let cpWrap = Wrap(100)
  borrowAny(cpWrap)
  let _ = cpWrap
  let _ = cpWrap

  let ncWrap = Wrap(FileDescriptor()) // expected-error {{'ncWrap' consumed more than once}}
  borrowAny(ncWrap)
  let _ = ncWrap // expected-note {{consumed}}
  let _ = ncWrap // expected-note {{consumed}}
}

func test1<T: ~Copyable>(_ t: consuming T, // expected-error {{'t' consumed more than once}}
                                           // expected-error@-1 {{'t' used after consume}}
                         _ borrowArg: borrowing T) -> Wrap<T> {
                         // expected-error@-1 {{'borrowArg' is borrowed and cannot be consumed}}
  borrowAny(t)
  borrowAny(borrowArg)

  let wrap1 = Wrap(t) // expected-note {{consumed}}
  let _ = Wrap(t)     // expected-note 2{{consumed}}

  let _ = Wrap(borrowArg) // expected-note {{consumed}}

  borrowAny(t) // expected-note {{used}}
  borrowAny(borrowArg)

  return wrap1
}

func testWrap<T: ~Copyable>(_ x: borrowing Wrap<T>,
                             // expected-error@-1 {{'x' is borrowed and cannot be consumed}}
                            _ y: consuming Wrap<T>,
                            _ new: () -> T) -> T {
  _ = x.item // expected-note {{consumed}}

  // expected-error@+1 {{'result' consumed more than once}}
  let result = { (v: inout Wrap<T>) -> T in
   let result = v.item
   v.item = new()
   return result
  }(&y)

  let _ = result // expected-note {{consumed}}

  return result // expected-note {{consumed}}
}
