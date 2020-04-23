// RUN: %target-typecheck-verify-swift

func acceptsInt(_ x: Int) {}
protocol Proto {}
func acceptsProto(_ x: Proto) {}
func acceptsIntAutoclosure(_ x: @autoclosure () -> Int) {}
func acceptsInoutInt(_ x: inout Int) {}

let x: Int = fatalError()
let y: (Int, String) = (1, fatalError())
let yy: (Int, String) = fatalError()
let z: Int? = nil
let zz = z ?? fatalError()

let anyClass: AnyClass = fatalError()
let anyObject: AnyObject = fatalError()
let any: Any = fatalError()

_ = (fatalError() as CustomStringConvertible).description
_ = (fatalError() as [Int]).map {$0 + 1}

acceptsInt(fatalError())
let a: Int = fatalError() + fatalError()

acceptsProto(fatalError())

(fatalError() as ()->Void)()

_ = fatalError() == (fatalError() as String)

let singleExprClosure: ()->String = { fatalError() }
let closure: ()->String = { return fatalError() }

let ternaryResult: Int = true ? 1 : fatalError()

_ = "\(fatalError())"

_ = try! (fatalError() as (Int) throws -> Int)(42)

let _: [Int?] = [1,2,3, nil, fatalError()]

let _: [String:Int] = ["" : 1, "a": 2, "b": fatalError()]

acceptsIntAutoclosure(fatalError())

var mutableVar = 1
acceptsInoutInt(&mutableVar)

if fatalError() {}
guard fatalError() else { fatalError() }
while fatalError() {}
repeat {} while fatalError()
switch 1 {
case fatalError():
  break
default:
  break
}

let cc: ()->Never = { fatalError() }
let c: ()->Int = cc

let f: (Int)->Void = {x in }
let ff: (Never)->Void = f

let g: (Int)->Never = {x in fatalError()}
let gg: (Never)->Int = g

let arr1: [Never] = []
var arr2: [Int] = arr1
for _ in arr1 {}
for _: Int in arr1 {}

let arrF: ([Int]) -> Never = { a in fatalError() }
let arrF2: (Never)->[Int] = arrF

let opt: Never? = nil
let opt2: Int? = opt
if let _ = opt {}

func overloaded(_ x: Never) {}
func overloaded(_ x: Int) {}
overloaded(fatalError())

func ambiguousOverload(_ x: Int) {} // expected-note {{found this candidate}}
func ambiguousOverload(_ x: String) {} // expected-note {{found this candidate}}
ambiguousOverload(fatalError()) // expected-error {{ambiguous use of 'ambiguousOverload'}}

class Super {
  func f(x: Never) -> Int { 42 }

  var x: Int { 42 }
}

class Sub: Super {
  override func f(x: Int?) -> Never { fatalError() }

  override var x: Never { fatalError() }
}

// For source compatibility, the following must continue to typecheck.
// The trailing closure argument to `acceptsCFuncPtr` will be rewritten into a
// `()->()` closure because @convention(c) functions can't have a covariant return type.
func acceptsCFuncPtr(_ f: @convention(c) ()->()) {
  f()
}
acceptsCFuncPtr {
  fatalError()
}

// Regular uninhabited types are not equivalent to the bottom type.
enum Uninhabited {}
func returnsUninhabited() -> Uninhabited {}
let uninhabited: String = returnsUninhabited() // expected-error {{cannot convert value of type 'Uninhabited' to specified type 'String'}}
