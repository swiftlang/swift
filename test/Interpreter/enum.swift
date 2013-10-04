// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

enum Singleton {
  case x(Int, Char)
}

enum NoPayload {
  case x
  case y
  case z
}

enum SinglePayloadTrivial {
  case x(Char, Int)
  case y
  case z
}

enum MultiPayloadTrivial {
  case x(Char, Int)
  case y(Int, Double)
  case z
}

var s = Singleton.x(1, 'a')
switch s {
case .x(var int, var char):
  // CHECK: 1
  println(int)
  // CHECK: a
  println(char)
}

func printNoPayload(v:NoPayload) {
  switch v {
  case .x:
    println("NoPayload.x")
  case .y:
    println("NoPayload.y")
  case .z:
    println("NoPayload.z")
  }
}

// CHECK: NoPayload.x
printNoPayload(.x)
// CHECK: NoPayload.y
printNoPayload(.y)
// CHECK: NoPayload.z
printNoPayload(.z)

func printSinglePayloadTrivial(v:SinglePayloadTrivial) {
  switch v {
  case .x(var char, var int):
    println("SinglePayloadTrivial.x(\(char), \(int))")
  case .y:
    println("SinglePayloadTrivial.y")
  case .z:
    println("SinglePayloadTrivial.z")
  }
}

// CHECK: SinglePayloadTrivial.x(b, 2)
printSinglePayloadTrivial(.x('b', 2))
// CHECK: SinglePayloadTrivial.y
printSinglePayloadTrivial(.y)
// CHECK: SinglePayloadTrivial.z
printSinglePayloadTrivial(.z)

func printMultiPayloadTrivial(v:MultiPayloadTrivial) {
  switch v {
  case .x(var char, var int):
    println("MultiPayloadTrivial.x(\(char), \(int))")
  case .y(var int, var double):
    println("MultiPayloadTrivial.y(\(int), \(double))")
  case .z:
    println("MultiPayloadTrivial.z")
  }
}

// CHECK: MultiPayloadTrivial.x(c, 3)
printMultiPayloadTrivial(.x('c', 3))
// CHECK: MultiPayloadTrivial.y(4, 5.5)
printMultiPayloadTrivial(.y(4, 5.5))
// CHECK: MultiPayloadTrivial.z
printMultiPayloadTrivial(.z)

protocol Runcible {
  func runce()
}

struct Spoon : Runcible {
  var xxx : Int
  func runce() { println("Spoon!") }
}

struct Hat : Runcible {
  var xxx : Float
  func runce() { println("Hat!") }
}

enum SinglePayloadAddressOnly {
  case x(Runcible)
  case y
}

func printSinglePayloadAddressOnly(v:SinglePayloadAddressOnly) {
  switch v {
  case .x(var runcible):
    runcible.runce()
  case .y:
    println("Why?")
  }
}

// FIXME: Explicit 'as Runcible' conversions because of
// <rdar://problem/14885865>

// CHECK: Spoon!
var runSpoon : Runcible = Spoon()
printSinglePayloadAddressOnly(.x(runSpoon))
// CHECK: Hat!
var runHat : Runcible = Hat()
printSinglePayloadAddressOnly(.x(runHat))
// CHECK: Why?
printSinglePayloadAddressOnly(.y)

enum MultiPayloadAddressOnly {
  case x(Runcible)
  case y(String, Runcible)
  case z
}

func printMultiPayloadAddressOnly(v:MultiPayloadAddressOnly) {
  switch v {
  case .x(var runcible):
    runcible.runce()
  case .y(var s, var runcible):
    print("\(s) ")
    runcible.runce()
  case .z:
    println("Zed.")
  }
}

// CHECK: Spoon!
printMultiPayloadAddressOnly(.x(runSpoon))
// CHECK: Porkpie Hat!
printMultiPayloadAddressOnly(.y("Porkpie", runHat))
// CHECK: Zed.
printMultiPayloadAddressOnly(.z)

enum TrivialGeneric<T, U> {
  case x(T, U)
}

func unwrapTrivialGeneric<T, U>(tg:TrivialGeneric<T, U>) -> (T, U) {
  switch tg {
  case .x(var t, var u):
    return (t, u)
  }
}

func wrapTrivialGeneric<T, U>(t:T, u:U) -> TrivialGeneric<T, U> {
  // FIXME: full qualification required because of <rdar://problem/14994273>
  return TrivialGeneric<T, U>.x(t, u)
}

var tg : TrivialGeneric<Int, String> = .x(23, "skidoo")
// CHECK: 23 skidoo
switch tg {
case .x(var t, var u):
  println("\(t) \(u)")
}

// CHECK: 413 dream
switch unwrapTrivialGeneric(.x(413, "dream")) {
case (var t, var u):
  println("\(t) \(u)")
}

// CHECK: 1 is the loneliest number that you'll ever do
switch wrapTrivialGeneric(1, "is the loneliest number that you'll ever do") {
case .x(var t, var u):
  println("\(t) \(u)")
}

enum Ensemble<S:Runcible, H:Runcible> {
  case x(S, H)
}

func concreteEnsemble(e:Ensemble<Spoon, Hat>) {
  switch e {
  case .x(var spoon, var hat):
    spoon.runce()
    hat.runce()
  }
}

func genericEnsemble<T:Runcible, U:Runcible>(e:Ensemble<T, U>) {
  switch e {
  case .x(var t, var u):
    t.runce()
    u.runce()
  }
}

// CHECK: Spoon!
// CHECK: Hat!
concreteEnsemble(.x(Spoon(), Hat()))
// CHECK: Spoon!
// CHECK: Hat!
genericEnsemble(.x(Spoon(), Hat()))
// CHECK: Spoon!
// CHECK: Spoon!
genericEnsemble(.x(Spoon(), Spoon()))
// CHECK: Hat!
// CHECK: Spoon!
genericEnsemble(.x(Hat(), Spoon()))

enum Optionable<T> {
  case Mere(T)
  case Nought

  init() { self = .Nought }
}

func tryRunce<T:Runcible>(x:Optionable<T>) {
  switch x {
  case .Mere(var r):
    r.runce()
  case .Nought:
    println("nought")
  }
}

// CHECK: Spoon!
tryRunce(.Mere(Spoon()))
// CHECK: Hat!
tryRunce(.Mere(Hat()))
// CHECK: nought
tryRunce(Optionable<Spoon>.Nought)
// CHECK: nought
tryRunce(Optionable<Hat>.Nought)

func optionableInts() {
  var optionables = new Optionable<Int>[4]
  optionables[0] = .Mere(219)
  optionables[1] = .Nought
  optionables[2] = .Nought
  optionables[3] = .Mere(20721)

  for o in optionables {
    switch o {
    case .Mere(var x):
      println(x)
    case .Nought:
      println("---")
    }
  }
}
// CHECK: 219
// CHECK: ---
// CHECK: ---
// CHECK: 20721
optionableInts()

func optionableRuncibles<T:Runcible>(x:T) {
  var optionables = new Optionable<T>[4]
  optionables[0] = .Mere(x)
  optionables[1] = .Nought
  optionables[2] = .Mere(x)
  optionables[3] = .Nought

  for o in optionables {
    switch o {
    case .Mere(var x):
      x.runce()
    case .Nought:
      println("---")
    }
  }
}
// CHECK: Spoon!
// CHECK: ---
// CHECK: Spoon!
// CHECK: ---
optionableRuncibles(Spoon())
// CHECK: Hat!
// CHECK: ---
// CHECK: Hat!
// CHECK: ---
optionableRuncibles(Hat())
