// RUN: %swift -parse -parse-stdlib -verify %s

// This disables importing the stdlib intentionally.

operator infix == {
  associativity left
  precedence 110
}

operator infix & {
  associativity left
  precedence 150
}

operator infix => {
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

operator postfix *!* {}
operator prefix *!* {}

struct LOOK {}
struct LOOKBang {
  func exclaim() {}
}

@postfix func *!* (x: LOOK) -> LOOKBang {}
@prefix func *!* (x: LOOKBang) {}

func test2() {
  *!*LOOK()*!*
}

// This should be parsed as (x*!*).exclaim()
LOOK()*!*.exclaim()


operator prefix ^ {}
operator infix ^ {}
operator postfix ^ {}

@postfix func ^ (x: God) -> TheDevil {}
@prefix func ^ (x: TheDevil) -> God {}

func ^ (x: TheDevil, y: God) -> Man {} // expected-note 2{{in initialization of parameter 'x'}}

var _ : TheDevil = God()^
var _ : God = ^TheDevil()
var _ : Man = TheDevil() ^ God()
var _ : Man = God()^ ^ ^TheDevil()
var _ = God()^TheDevil() // expected-error{{'God' is not convertible to 'TheDevil'}}

@postfix func ^ (x: Man) -> () -> God {
  return { return God() }
}

var _ : God = Man()^() // expected-error{{'Man' is not convertible to 'TheDevil'}}

func &(x : Man, y : Man) -> Man { return x } // forgive amp_prefix token

operator prefix ⚽️ {}

@prefix func ⚽️(x: Man) { }
