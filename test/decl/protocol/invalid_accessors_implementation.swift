// RUN: %target-typecheck-verify-swift


//===---
// https://github.com/apple/swift/issues/56360
//===---

protocol Protocol1 {
  var a: Int { get { 0 } } // expected-error {{protocol property getter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol2 {
  var a: Int { set { a = 0 } } // expected-error {{protocol property setter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol3 {
  var a: Int { get { 0 } set } // expected-error {{protocol property getter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol4 {
  var a: Int { get { 0 } set { a = 0 } } // expected-error {{protocol property getter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol5 {
  var a: Int { get set willSet { print("HI")} } // expected-error {{expected get or set in a protocol property}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol6 {
  var a: Int { get { 0 } set willSet { print("HI")} } // expected-error {{protocol property getter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol7 {
  var a: Int { set { print("HI") } willSet { print("HI") } } // expected-error {{protocol property setter cannot have a default implementation specified here; use extension instead}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol8 {
  var a: Int { set willSet { print("HI") } } // expected-error {{expected get or set in a protocol property}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol9 {
  var a: Int { return 0 } // expected-error {{property in protocol must have explicit { get } or { get set } specifier}} expected-error {{expected get or set in a protocol property}}
}

protocol Protocol10 {
}
// No error in extension
extension Protocol10 {
  var a: Int { return 0 }
}