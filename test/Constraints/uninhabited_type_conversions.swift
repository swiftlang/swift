// RUN: %target-typecheck-verify-swift

enum Uninhabited {}
func returnsUninhabited() -> Uninhabited {}

func acceptsInt(_ x: Int) {}
protocol Proto {}
func acceptsProto(_ x: Proto) {}
func acceptsIntAutoclosure(_ x: @autoclosure () -> Int) {}
func acceptsInoutInt(_ x: inout Int) {}

let x: Int = returnsUninhabited()
let y: (Int, String) = (1, returnsUninhabited())
let yy: (Int, String) = returnsUninhabited()
let z: Int? = nil
let zz = z ?? returnsUninhabited()

acceptsInt(returnsUninhabited())
let a: Int = returnsUninhabited() + returnsUninhabited()

acceptsProto(returnsUninhabited())

(returnsUninhabited() as ()->Void)()

_ = returnsUninhabited() == (returnsUninhabited() as String)

let singleExprClosure: ()->String = { returnsUninhabited() }
let closure: ()->String = { return returnsUninhabited() }

let ternaryResult: Int = true ? 1 : returnsUninhabited()

_ = "\(returnsUninhabited())"

_ = try! (returnsUninhabited() as (Int) throws -> Int)(42)

let _: [Int?] = [1,2,3, nil, returnsUninhabited()]

let _: [String:Int] = ["" : 1, "a": 2, "b": returnsUninhabited()]

acceptsIntAutoclosure(returnsUninhabited())

var mutableVar = 1
acceptsInoutInt(&mutableVar)

if returnsUninhabited() {}
guard returnsUninhabited() else { returnsUninhabited() }
while returnsUninhabited() {}
repeat {} while returnsUninhabited()
switch 1 {
case returnsUninhabited():
  break
default:
  break
}

let cc: ()->Uninhabited = { returnsUninhabited() }
let c: ()->Int = cc

let f: (Int)->Void = {x in }
let ff: (Uninhabited)->Void = f

let g: (Int)->Uninhabited = {x in returnsUninhabited()}
let gg: (Uninhabited)->Int = g

func overloaded(_ x: Uninhabited) {}
func overloaded(_ x: Int) {}

overloaded(returnsUninhabited())

class Super {
  func f(_ x: Uninhabited) -> Int { 42 }
}

class Sub: Super {
  override func f(_ x: Int) -> Uninhabited {}
}
