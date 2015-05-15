// RUN: %target-parse-verify-swift -parse-stdlib

// This disables importing the stdlib intentionally.

infix operator == {
  associativity left
  precedence 110
}

infix operator & {
  associativity left
  precedence 150
}

infix operator => {
  associativity right
  precedence 100
}

struct Man {}
struct TheDevil {}
struct God {}

struct Five {}
struct Six {}
struct Seven {}

struct ManIsFive {}
struct TheDevilIsSix {}
struct GodIsSeven {}

struct TheDevilIsSixThenGodIsSeven {}

func == (x: Man, y: Five) -> ManIsFive {}
func == (x: TheDevil, y: Six) -> TheDevilIsSix {}
func == (x: God, y: Seven) -> GodIsSeven {}

func => (x: TheDevilIsSix, y: GodIsSeven) -> TheDevilIsSixThenGodIsSeven {}
func => (x: ManIsFive, y: TheDevilIsSixThenGodIsSeven) {}

func test1() {
  Man() == Five() => TheDevil() == Six() => God() == Seven()
}

postfix operator *!* {}
prefix operator *!* {}

struct LOOK {}
struct LOOKBang {
  func exclaim() {}
}

postfix func *!* (x: LOOK) -> LOOKBang {}
prefix func *!* (x: LOOKBang) {}

func test2() {
  *!*LOOK()*!*
}

// This should be parsed as (x*!*).exclaim()
LOOK()*!*.exclaim()


prefix operator ^ {}
infix operator ^ {}
postfix operator ^ {}

postfix func ^ (x: God) -> TheDevil {}
prefix func ^ (x: TheDevil) -> God {}

func ^ (x: TheDevil, y: God) -> Man {}

var _ : TheDevil = God()^
var _ : God = ^TheDevil()
var _ : Man = TheDevil() ^ God()
var _ : Man = God()^ ^ ^TheDevil()
let _ = God()^TheDevil() // expected-error{{binary operator '^' cannot be applied to operands of type 'God' and 'TheDevil'}}

postfix func ^ (x: Man) -> () -> God {
  return { return God() }
}

var _ : God = Man()^() // expected-error{{'Man' is not convertible to 'God'}}

func &(x : Man, y : Man) -> Man { return x } // forgive amp_prefix token

prefix operator ⚽️ {}

prefix func ⚽️(x: Man) { }

infix operator ?? {
  associativity right
  precedence 100
}

func ??(x: Man, y: TheDevil) -> TheDevil {
  return y
}

func test3(a: Man, b: Man, c: TheDevil) -> TheDevil {
  return a ?? b ?? c
}

// <rdar://problem/17821399> We don't parse infix operators bound on both
// sides that begin with ! or ? correctly yet.
infix operator !! {}

func !!(x: Man, y: Man) {}
let foo = Man()
let bar = TheDevil()
foo!!foo // expected-error{{should have optional type}} expected-error{{consecutive statements}}
foo??bar // expected-error{{broken standard library}} expected-error{{consecutive statements}}

