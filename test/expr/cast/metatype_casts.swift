// RUN: %target-parse-verify-swift

func use<T>(_: T) {}


class C {}
class D: C {}
class E: P {}
class X {}

protocol P {}
protocol Q {}
protocol CP: class {}

let any: Any.Type = Int.self
use(any as! Int.Type)
use(any as! C.Type)
use(any as! D.Type)
use(any as! AnyObject.Type)
use(any as! AnyObject.Protocol)
use(any as! P.Type)
use(any as! P.Protocol)

let anyP: Any.Protocol = Any.self
use(anyP is Any.Type) // expected-warning{{always true}}
use(anyP as! Int.Type) // TODO: always fails

let anyObj: AnyObject.Type = D.self
use(anyObj as! Int.Type) // TODO: always fails
use(anyObj as! C.Type)
use(anyObj as! D.Type)
use(anyObj as! AnyObject.Protocol) // TODO: always fails
use(anyObj as! P.Type)
use(anyObj as! P.Protocol) // TODO: always fails

let c: C.Type = D.self
use(c as! D.Type)
use(c as! X.Type) // expected-warning{{always fails}}
use(c is AnyObject.Type) // expected-warning{{always true}}
use(c as! AnyObject.Type) // expected-warning{{always succeeds}}
use(c as! AnyObject.Protocol) // expected-warning{{always fails}}
use(c as! CP.Type)
use(c as! CP.Protocol) // expected-warning{{always fails}}
use(c as! Int.Type) // expected-warning{{always fails}}

use(C.self as AnyObject.Protocol) // expected-error{{'C.Type' is not convertible to 'AnyObject.Protocol'}}
use(C.self as AnyObject.Type)
use(C.self as P.Type) // expected-error{{'C.Type' is not convertible to 'P.Type'; did you mean to use 'as!' to force downcast?}}

use(E.self as P.Protocol) // expected-error{{'E.Type' is not convertible to 'P.Protocol'}}
use(E.self as P.Type)
