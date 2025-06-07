// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-all-function-bodies
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-non-inlinable-function-bodies-without-types
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-non-inlinable-function-bodies

// REQUIRES: swift_swift_parser

// Make sure we properly handle `/.../` regex literals in skipped function
// bodies. Currently we detect them and avoid skipping, but in the future we
// ought to be able to skip over them.

prefix operator ^^
prefix func ^^ <T>(_ x: T) -> T { x }

struct A {
  static let r = /test":"(.*?)"/
}
struct B {
  static let r = /x*/
}

struct C {
  func foo() {
    let r = /x*/
  }
}

struct D {
  func foo() {
    func bar() {
      let r = /x}}*/
    }
  }
}

func a() { _ = /abc}}*/ }
func b() { _ = /\// }
func c() { _ = /\\/ }
func d() { _ = ^^/x}}*/ }
func e() { _ = (^^/x}}*/) }
func f() { _ = ^^/^x}}*/ }
func g() { _ = "\(/x}}*/)" }
func h() { _ = "\(^^/x}}*/)" }

func i() {
  func foo<T>(_ x: T, y: T) {}
  foo(/}}*/, y: /"/)
}

func j() {
  _ = {
    0
    /x}}}/ 
    2
  }
}

func k() {
  _ = 2
  / 1 / .bitWidth
}
func l() {
  _ = 2
  /x}*/ .self
}
func m() {
  _ = 2
  / 1 /
    .bitWidth
}
func n() {
  _ = 2
  /x}/
    .bitWidth
}
func o() {
  _ = /x// comment
}
func p() {
  _ = /x // comment
}
func q() {
  _ = /x/*comment*/
}
func r() { _ = /[(0)]/ }
func s() { _ = /(x)/ }
func t() { _ = /[)]/ }
func u() { _ = /[a\])]/ }
func v() { _ = /([)])/ }
func w() { _ = /]]][)]/ }

func x() { _ = /,/ }
func y() { _ = /}/ }
func z() { _ = /]/ }
func a1() { _ = /:/ }
func a2() { _ = /;/ }
func a3() { _ = /)/ }
func a4() { _ = / / } // expected-error {{regex literal may not start with space; add backslash to escape}}
func a5() { _ = /\ / }

prefix operator /
prefix func / <T> (_ x: T) -> T { x }

enum E {
  case e
  func foo<T>(_ x: T) {}
}

func a7() { _ = /\/}/ }

// Make sure we don't emit errors for these.
func err1() { _ = /0xG/ }
func err2() { _ = /0oG/ }
func err3() { _ = /"/ }
func err4() { _ = /'/ }
func err5() { _ = /<#placeholder#>/ }

func err6() { _ = ^^/0xG/ }
func err7() { _ = ^^/0oG/ }
func err8() { _ = ^^/"/ }
func err9() { _ = ^^/'/ }
func err10() { _ = ^^/<#placeholder#>/ }

func err11() { _ = (^^/0xG/) }
func err12() { _ = (^^/0oG/) }
func err13() { _ = (^^/"/) }
func err14() { _ = (^^/'/) }
func err15() { _ = (^^/<#placeholder#>/) }
