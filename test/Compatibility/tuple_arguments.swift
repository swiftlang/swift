// RUN: %target-typecheck-verify-swift -swift-version 3

// Tests for tuple argument behavior in Swift 3, which was broken.
// The Swift 4 test is in test/Constraints/tuple_arguments.swift.

// Key:
// - "Crashes in actual Swift 3" -- snippets which crashed in Swift 3.0.1.
//   These don't have well-defined semantics in Swift 3 mode and don't
//   matter for source compatibility purposes.
//
// - "Does not diagnose in Swift 3 mode" -- snippets which failed to typecheck
//   in Swift 3.0.1, but now typecheck. This is fine.
//
// - "Diagnoses in Swift 3 mode" -- snippets which typechecked in Swift 3.0.1,
//   but now fail to typecheck. These are bugs in Swift 3 mode that should be
//   fixed.
//
// - "Crashes in Swift 3 mode" -- snippets which did not crash Swift 3.0.1,
//   but now crash. These are bugs in Swift 3 mode that should be fixed.

func concrete(_ x: Int) {}
func concreteLabeled(x: Int) {}
func concreteTwo(_ x: Int, _ y: Int) {} // expected-note 3 {{'concreteTwo' declared here}}
func concreteTuple(_ x: (Int, Int)) {}

do {
  concrete(3)
  concrete((3))

  concreteLabeled(x: 3)
  concreteLabeled(x: (3))
  concreteLabeled((x: 3)) // expected-error {{missing argument label 'x:' in call}}

  concreteTwo(3, 4)
  concreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  concreteTuple(3, 4) // expected-error {{extra argument in call}}
  concreteTuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  concrete(a)
  concrete((a))
  concrete(c)

  concreteTwo(a, b)
  concreteTwo((a, b))
  concreteTwo(d) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  concreteTuple(a, b)
  concreteTuple((a, b))
  concreteTuple(d)
}

do {
  var a = 3 // expected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4 // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var c = (3) // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}
  var d = (a, b) // expected-warning {{variable 'd' was never mutated; consider changing to 'let' constant}}

  concrete(a)
  concrete((a))
  concrete(c)

  concreteTwo(a, b)
  concreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  concreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  concreteTuple(a, b) // expected-error {{extra argument in call}}
  concreteTuple((a, b))
  concreteTuple(d)
}

func generic<T>(_ x: T) {}
func genericLabeled<T>(x: T) {}
func genericTwo<T, U>(_ x: T, _ y: U) {} // expected-note 5 {{'genericTwo' declared here}}
func genericTuple<T, U>(_ x: (T, U)) {}

do {
  generic(3)
  generic(3, 4)
  generic((3))
  generic((3, 4))

  genericLabeled(x: 3)
  genericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  genericLabeled(x: (3))
  genericLabeled(x: (3, 4))

  genericTwo(3, 4)
  genericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(3, 4) // expected-error {{extra argument in call}}
  genericTuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  generic(a)
  generic(a, b)
  generic((a))
  generic(c)
  generic((a, b))
  generic(d)

  genericTwo(a, b)
  genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(a, b) // expected-error {{extra argument in call}}
  genericTuple((a, b))
  genericTuple(d)
}

do {
  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  generic(a)
  // generic(a, b) // Crashes in actual Swift 3
  generic((a))
  generic(c)
  generic((a, b))
  generic(d)

  genericTwo(a, b)
  genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(a, b) // expected-error {{extra argument in call}}
  genericTuple((a, b))
  genericTuple(d)
}

var function: (Int) -> ()
var functionTwo: (Int, Int) -> () // expected-note 3 {{'functionTwo' declared here}}
var functionTuple: ((Int, Int)) -> ()

do {
  function(3)
  function((3))

  functionTwo(3, 4)
  functionTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  functionTuple(3, 4) // expected-error {{extra argument in call}}
  functionTuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  function(a)
  function((a))
  function(c)

  functionTwo(a, b)
  functionTwo((a, b))
  functionTwo(d) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  functionTuple(a, b)
  functionTuple((a, b))
  functionTuple(d)
}

do {
  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  function(a)
  function((a))
  function(c)

  functionTwo(a, b)
  functionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  functionTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  functionTuple(a, b) // expected-error {{extra argument in call}}
  functionTuple((a, b))
  functionTuple(d)
}

struct Concrete {}

extension Concrete {
  func concrete(_ x: Int) {}
  func concreteTwo(_ x: Int, _ y: Int) {} // expected-note 3 {{'concreteTwo' declared here}}
  func concreteTuple(_ x: (Int, Int)) {}
}

do {
  let s = Concrete()

  s.concrete(3)
  s.concrete((3))

  s.concreteTwo(3, 4)
  s.concreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.concreteTuple(3, 4) // expected-error {{extra argument in call}}
  s.concreteTuple((3, 4))
}

do {
  let s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.concrete(a)
  s.concrete((a))
  s.concrete(c)

  s.concreteTwo(a, b)
  s.concreteTwo((a, b))
  s.concreteTwo(d) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  s.concreteTuple(a, b)
  s.concreteTuple((a, b))
  s.concreteTuple(d)
}

do {
  var s = Concrete()

  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  s.concrete(a)
  s.concrete((a))
  s.concrete(c)

  s.concreteTwo(a, b)
  s.concreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.concreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.concreteTuple(a, b) // expected-error {{extra argument in call}}
  s.concreteTuple((a, b))
  s.concreteTuple(d)
}

extension Concrete {
  func generic<T>(_ x: T) {}
  func genericLabeled<T>(x: T) {}
  func genericTwo<T, U>(_ x: T, _ y: U) {} // expected-note 5 {{'genericTwo' declared here}}
  func genericTuple<T, U>(_ x: (T, U)) {}
}

do {
  let s = Concrete()

  s.generic(3)
  s.generic(3, 4)
  s.generic((3))
  s.generic((3, 4))

  s.genericLabeled(x: 3)
  s.genericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  s.genericLabeled(x: (3))
  s.genericLabeled(x: (3, 4))

  s.genericTwo(3, 4)
  s.genericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(3, 4) // expected-error {{extra argument in call}}
  s.genericTuple((3, 4))
}

do {
  let s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.generic(a)
  s.generic(a, b)
  s.generic((a))
  s.generic((a, b))
  s.generic(d)

  s.genericTwo(a, b)
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{extra argument in call}}
  s.genericTuple((a, b))
  s.genericTuple(d)
}

do {
  var s = Concrete()

  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  s.generic(a)
  // s.generic(a, b) // Crashes in actual Swift 3
  s.generic((a))
  s.generic((a, b))
  s.generic(d)

  s.genericTwo(a, b)
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{extra argument in call}}
  s.genericTuple((a, b))
  s.genericTuple(d)
}

extension Concrete {
  mutating func mutatingConcrete(_ x: Int) {}
  mutating func mutatingConcreteTwo(_ x: Int, _ y: Int) {} // expected-note 3 {{'mutatingConcreteTwo' declared here}}
  mutating func mutatingConcreteTuple(_ x: (Int, Int)) {}
}

do {
  var s = Concrete()

  s.mutatingConcrete(3)
  s.mutatingConcrete((3))

  s.mutatingConcreteTwo(3, 4)
  s.mutatingConcreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingConcreteTuple(3, 4) // expected-error {{extra argument in call}}
  s.mutatingConcreteTuple((3, 4))
}

do {
  var s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.mutatingConcrete(a)
  s.mutatingConcrete((a))
  s.mutatingConcrete(c)

  s.mutatingConcreteTwo(a, b)
  s.mutatingConcreteTwo((a, b))
  s.mutatingConcreteTwo(d) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  s.mutatingConcreteTuple(a, b)
  s.mutatingConcreteTuple((a, b))
  s.mutatingConcreteTuple(d)
}

do {
  var s = Concrete()

  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  s.mutatingConcrete(a)
  s.mutatingConcrete((a))
  s.mutatingConcrete(c)

  s.mutatingConcreteTwo(a, b)
  s.mutatingConcreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingConcreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingConcreteTuple(a, b) // expected-error {{extra argument in call}}
  s.mutatingConcreteTuple((a, b))
  s.mutatingConcreteTuple(d)
}

extension Concrete {
  mutating func mutatingGeneric<T>(_ x: T) {}
  mutating func mutatingGenericLabeled<T>(x: T) {}
  mutating func mutatingGenericTwo<T, U>(_ x: T, _ y: U) {} // expected-note 5 {{'mutatingGenericTwo' declared here}}
  mutating func mutatingGenericTuple<T, U>(_ x: (T, U)) {}
}

do {
  var s = Concrete()

  s.mutatingGeneric(3)
  s.mutatingGeneric(3, 4)
  s.mutatingGeneric((3))
  s.mutatingGeneric((3, 4))

  s.mutatingGenericLabeled(x: 3)
  s.mutatingGenericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  s.mutatingGenericLabeled(x: (3))
  s.mutatingGenericLabeled(x: (3, 4))

  s.mutatingGenericTwo(3, 4)
  s.mutatingGenericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(3, 4) // expected-error {{extra argument in call}}
  s.mutatingGenericTuple((3, 4))
}

do {
  var s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.mutatingGeneric(a)
  s.mutatingGeneric(a, b)
  s.mutatingGeneric((a))
  s.mutatingGeneric((a, b))
  s.mutatingGeneric(d)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingGenericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{extra argument in call}}
  s.mutatingGenericTuple((a, b))
  s.mutatingGenericTuple(d)
}

do {
  var s = Concrete()

  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  s.mutatingGeneric(a)
  // s.mutatingGeneric(a, b) // Crashes in actual Swift 3
  s.mutatingGeneric((a))
  s.mutatingGeneric((a, b))
  s.mutatingGeneric(d)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingGenericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{extra argument in call}}
  s.mutatingGenericTuple((a, b))
  s.mutatingGenericTuple(d)
}

extension Concrete {
  var function: (Int) -> () { return concrete }
  var functionTwo: (Int, Int) -> () { return concreteTwo }
  var functionTuple: ((Int, Int)) -> () { return concreteTuple }
}

do {
  let s = Concrete()

  s.function(3)
  s.function((3))

  s.functionTwo(3, 4)
  s.functionTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.functionTuple(3, 4) // expected-error {{extra argument in call}}
  s.functionTuple((3, 4))
}

do {
  let s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.function(a)
  s.function((a))
  s.function(c)

  s.functionTwo(a, b)
  s.functionTwo((a, b))
  s.functionTwo(d) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  s.functionTuple(a, b)
  s.functionTuple((a, b))
  s.functionTuple(d)
}

do {
  var s = Concrete()

  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  s.function(a)
  s.function((a))
  s.function(c)

  s.functionTwo(a, b)
  s.functionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.functionTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.functionTuple(a, b) // expected-error {{extra argument in call}}
  s.functionTuple((a, b))
  s.functionTuple(d)
}

struct InitTwo {
  init(_ x: Int, _ y: Int) {} // expected-note 3 {{'init' declared here}}
}

struct InitTuple {
  init(_ x: (Int, Int)) {}
}

struct InitLabeledTuple {
  init(x: (Int, Int)) {}
}

do {
  _ = InitTwo(3, 4)
  _ = InitTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = InitTuple(3, 4) // expected-error {{extra argument in call}}
  _ = InitTuple((3, 4))

  _ = InitLabeledTuple(x: 3, 4) // expected-error {{extra argument in call}}
  _ = InitLabeledTuple(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = InitTwo(a, b)
  _ = InitTwo((a, b))
  _ = InitTwo(c) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  _ = InitTuple(a, b)
  _ = InitTuple((a, b))
  _ = InitTuple(c)
}

do {
  var a = 3 // expected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4 // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var c = (a, b) // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}

  _ = InitTwo(a, b)
  _ = InitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = InitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = InitTuple(a, b) // expected-error {{extra argument in call}}
  _ = InitTuple((a, b))
  _ = InitTuple(c)
}

struct SubscriptTwo {
  subscript(_ x: Int, _ y: Int) -> Int { get { return 0 } set { } } // expected-note 3 {{'subscript' declared here}}
}

struct SubscriptTuple {
  subscript(_ x: (Int, Int)) -> Int { get { return 0 } set { } }
}

struct SubscriptLabeledTuple {
  subscript(x x: (Int, Int)) -> Int { get { return 0 } set { } }
}

do {
  let s1 = SubscriptTwo()
  _ = s1[3, 4]
  _ = s1[(3, 4)] // expected-error {{missing argument for parameter #2 in call}}

  let s2 = SubscriptTuple()
  _ = s2[3, 4] // expected-error {{extra argument in call}}
  _ = s2[(3, 4)]

  let s3 = SubscriptLabeledTuple()
  _ = s3[x: 3, 4] // expected-error {{extra argument in call}}
  _ = s3[x: (3, 4)]
}

do {
  let a = 3
  let b = 4
  let d = (a, b)

  let s1 = SubscriptTwo()
  _ = s1[a, b]
  _ = s1[(a, b)]
  _ = s1[d]

  let s2 = SubscriptTuple()
  _ = s2[a, b]
  _ = s2[(a, b)]
  _ = s2[d]
}

do {
  var a = 3 // expected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4 // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var d = (a, b) // expected-warning {{variable 'd' was never mutated; consider changing to 'let' constant}}

  var s1 = SubscriptTwo()
  _ = s1[a, b]
  _ = s1[(a, b)] // expected-error {{missing argument for parameter #2 in call}}
  _ = s1[d] // expected-error {{missing argument for parameter #2 in call}}

  var s2 = SubscriptTuple()
  _ = s2[a, b] // expected-error {{extra argument in call}}}
  _ = s2[(a, b)]
  _ = s2[d]
}

enum Enum {
  case two(Int, Int) // expected-note 3 {{'two' declared here}}
  case tuple((Int, Int))
  case labeledTuple(x: (Int, Int))
}

do {
  _ = Enum.two(3, 4)
  _ = Enum.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = Enum.tuple(3, 4) // expected-error {{extra argument in call}}
  _ = Enum.tuple((3, 4))

  _ = Enum.labeledTuple(x: 3, 4) // expected-error {{extra argument in call}}
  _ = Enum.labeledTuple(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = Enum.two(a, b)
  _ = Enum.two((a, b))
  _ = Enum.two(c) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  _ = Enum.tuple(a, b)
  _ = Enum.tuple((a, b))
  _ = Enum.tuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = Enum.two(a, b)
  _ = Enum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = Enum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = Enum.tuple(a, b) // expected-error {{extra argument in call}}
  _ = Enum.tuple((a, b))
  _ = Enum.tuple(c)
}

struct Generic<T> {}

extension Generic {
  func generic(_ x: T) {}
  func genericLabeled(x: T) {}
  func genericTwo(_ x: T, _ y: T) {} // expected-note 2 {{'genericTwo' declared here}}
  func genericTuple(_ x: (T, T)) {}
}

do {
  let s = Generic<Double>()

  s.generic(3.0)
  s.generic((3.0))

  s.genericLabeled(x: 3.0)
  s.genericLabeled(x: (3.0))

  s.genericTwo(3.0, 4.0)
  s.genericTwo((3.0, 4.0)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(3.0, 4.0) // expected-error {{extra argument in call}}
  s.genericTuple((3.0, 4.0))

  let sTwo = Generic<(Double, Double)>()

  sTwo.generic(3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.generic((3.0, 4.0))

  sTwo.genericLabeled(x: 3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.genericLabeled(x: (3.0, 4.0))
}

do {
  let s = Generic<Double>()

  let a = 3.0
  let b = 4.0
  let c = (3.0)
  let d = (a, b)

  s.generic(a)
  s.generic((a))
  s.generic(c)

  s.genericTwo(a, b)
  s.genericTwo((a, b))

  s.genericTuple(a, b)
  s.genericTuple((a, b))

  let sTwo = Generic<(Double, Double)>()

  sTwo.generic(a, b)
  sTwo.generic((a, b))
  sTwo.generic(d)
}

do {
  var s = Generic<Double>()

  var a = 3.0
  var b = 4.0
  var c = (3.0)
  var d = (a, b)

  s.generic(a)
  s.generic((a))
  s.generic(c)

  s.genericTwo(a, b)
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{extra argument in call}}
  s.genericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.generic(a, b) // expected-error {{extra argument in call}}
  sTwo.generic((a, b))
  sTwo.generic(d)
}

extension Generic {
  mutating func mutatingGeneric(_ x: T) {}
  mutating func mutatingGenericLabeled(x: T) {}
  mutating func mutatingGenericTwo(_ x: T, _ y: T) {} // expected-note 2 {{'mutatingGenericTwo' declared here}}
  mutating func mutatingGenericTuple(_ x: (T, T)) {}
}

do {
  var s = Generic<Double>()

  s.mutatingGeneric(3.0)
  s.mutatingGeneric((3.0))

  s.mutatingGenericLabeled(x: 3.0)
  s.mutatingGenericLabeled(x: (3.0))

  s.mutatingGenericTwo(3.0, 4.0)
  s.mutatingGenericTwo((3.0, 4.0)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(3.0, 4.0) // expected-error {{extra argument in call}}
  s.mutatingGenericTuple((3.0, 4.0))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.mutatingGeneric((3.0, 4.0))

  sTwo.mutatingGenericLabeled(x: 3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.mutatingGenericLabeled(x: (3.0, 4.0))
}

do {
  var s = Generic<Double>()

  let a = 3.0
  let b = 4.0
  let c = (3.0)
  let d = (a, b)

  s.mutatingGeneric(a)
  s.mutatingGeneric((a))
  s.mutatingGeneric(c)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b))

  s.mutatingGenericTuple(a, b)
  s.mutatingGenericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(a, b)
  sTwo.mutatingGeneric((a, b))
  sTwo.mutatingGeneric(d)
}

do {
  var s = Generic<Double>()

  var a = 3.0
  var b = 4.0
  var c = (3.0)
  var d = (a, b)

  s.mutatingGeneric(a)
  s.mutatingGeneric((a))
  s.mutatingGeneric(c)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{extra argument in call}}
  s.mutatingGenericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(a, b) // expected-error {{extra argument in call}}
  sTwo.mutatingGeneric((a, b))
  sTwo.mutatingGeneric(d)
}

extension Generic {
  var genericFunction: (T) -> () { return generic }
  var genericFunctionTwo: (T, T) -> () { return genericTwo }
  var genericFunctionTuple: ((T, T)) -> () { return genericTuple }
}

do {
  let s = Generic<Double>()

  s.genericFunction(3.0)
  s.genericFunction((3.0))

  s.genericFunctionTwo(3.0, 4.0)
  s.genericFunctionTwo((3.0, 4.0)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericFunctionTuple(3.0, 4.0) // expected-error {{extra argument in call}}
  s.genericFunctionTuple((3.0, 4.0))

  let sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(3.0, 4.0)
  sTwo.genericFunction((3.0, 4.0)) // Does not diagnose in Swift 3 mode
}

do {
  let s = Generic<Double>()

  let a = 3.0
  let b = 4.0
  let c = (3.0)
  let d = (a, b)

  s.genericFunction(a)
  s.genericFunction((a))
  s.genericFunction(c)

  s.genericFunctionTwo(a, b)
  s.genericFunctionTwo((a, b))

  s.genericFunctionTuple(a, b)
  s.genericFunctionTuple((a, b))

  let sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(a, b)
  sTwo.genericFunction((a, b))
  sTwo.genericFunction(d) // Does not diagnose in Swift 3 mode
}

do {
  var s = Generic<Double>()

  var a = 3.0
  var b = 4.0
  var c = (3.0)
  var d = (a, b)

  s.genericFunction(a)
  s.genericFunction((a))
  s.genericFunction(c)

  s.genericFunctionTwo(a, b)
  s.genericFunctionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericFunctionTuple(a, b) // expected-error {{extra argument in call}}
  s.genericFunctionTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(a, b)
  sTwo.genericFunction((a, b)) // Does not diagnose in Swift 3 mode
  sTwo.genericFunction(d) // Does not diagnose in Swift 3 mode
}

struct GenericInit<T> { // expected-note 2 {{'T' declared as parameter to type 'GenericInit'}}
  init(_ x: T) {}
}

struct GenericInitLabeled<T> {
  init(x: T) {}
}

struct GenericInitTwo<T> {
  init(_ x: T, _ y: T) {} // expected-note 8 {{'init' declared here}}
}

struct GenericInitTuple<T> {
  init(_ x: (T, T)) {}
}

struct GenericInitLabeledTuple<T> {
  init(x: (T, T)) {}
}

do {
  _ = GenericInit(3, 4)
  _ = GenericInit((3, 4))

  _ = GenericInitLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeled(x: (3, 4))

  _ = GenericInitTwo(3, 4)
  _ = GenericInitTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitTuple((3, 4))

  _ = GenericInitLabeledTuple(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeledTuple(x: (3, 4))
}

do {
  _ = GenericInit<(Int, Int)>(3, 4)
  _ = GenericInit<(Int, Int)>((3, 4)) // expected-error {{expression type 'GenericInit<(Int, Int)>' is ambiguous without more context}}

  _ = GenericInitLabeled<(Int, Int)>(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeled<(Int, Int)>(x: (3, 4))

  _ = GenericInitTwo<Int>(3, 4)
  _ = GenericInitTwo<Int>((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple<Int>(3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitTuple<Int>((3, 4))

  _ = GenericInitLabeledTuple<Int>(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeledTuple<Int>(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericInit(a, b)
  _ = GenericInit((a, b))
  _ = GenericInit(c)

  _ = GenericInitTwo(a, b)
  _ = GenericInitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(a, b) // expected-error {{extra argument in call}}
  _ = GenericInitTuple((a, b))
  _ = GenericInitTuple(c)
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericInit<(Int, Int)>(a, b)
  _ = GenericInit<(Int, Int)>((a, b))
  _ = GenericInit<(Int, Int)>(c)

  _ = GenericInitTwo<Int>(a, b)
  _ = GenericInitTwo<Int>((a, b)) // Does not diagnose in Swift 3 mode
  _ = GenericInitTwo<Int>(c) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  _ = GenericInitTuple<Int>(a, b) // Does not diagnose in Swift 3 mode
  _ = GenericInitTuple<Int>((a, b))
  _ = GenericInitTuple<Int>(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = GenericInit(a, b) // expected-error {{extra argument in call}}
  _ = GenericInit((a, b)) // expected-error {{generic parameter 'T' could not be inferred}} // expected-note {{explicitly specify the generic arguments to fix this issue}}
  _ = GenericInit(c) // expected-error {{generic parameter 'T' could not be inferred}} // expected-note {{explicitly specify the generic arguments to fix this issue}}

  _ = GenericInitTwo(a, b)
  _ = GenericInitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(a, b) // expected-error {{extra argument in call}}
  _ = GenericInitTuple((a, b))
  _ = GenericInitTuple(c)
}

do {
  var a = 3 // expected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4 // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var c = (a, b) // expected-warning {{variable 'c' was never mutated; consider changing to 'let' constant}}

  // _ = GenericInit<(Int, Int)>(a, b) // Crashes in Swift 3
  _ = GenericInit<(Int, Int)>((a, b)) // expected-error {{expression type 'GenericInit<(Int, Int)>' is ambiguous without more context}}
   _ = GenericInit<(Int, Int)>(c) // expected-error {{expression type 'GenericInit<(Int, Int)>' is ambiguous without more context}}

  _ = GenericInitTwo<Int>(a, b)
  _ = GenericInitTwo<Int>((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo<Int>(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple<Int>(a, b) // expected-error {{extra argument in call}}
  _ = GenericInitTuple<Int>((a, b))
  _ = GenericInitTuple<Int>(c)
}

struct GenericSubscript<T> {
  subscript(_ x: T) -> Int { get { return 0 } set { } }
}

struct GenericSubscriptLabeled<T> {
  subscript(x x: T) -> Int { get { return 0 } set { } }
}

struct GenericSubscriptTwo<T> {
  subscript(_ x: T, _ y: T) -> Int { get { return 0 } set { } } // expected-note {{'subscript' declared here}}
}

struct GenericSubscriptTuple<T> {
  subscript(_ x: (T, T)) -> Int { get { return 0 } set { } }
}

struct GenericSubscriptLabeledTuple<T> {
  subscript(x x: (T, T)) -> Int { get { return 0 } set { } }
}

do {
  let s1 = GenericSubscript<(Double, Double)>()
  _ = s1[3.0, 4.0]
  _ = s1[(3.0, 4.0)] // expected-error {{expression type 'Int' is ambiguous without more context}}

  let s1a  = GenericSubscriptLabeled<(Double, Double)>()
  _ = s1a [x: 3.0, 4.0] // expected-error {{extra argument in call}}
  _ = s1a [x: (3.0, 4.0)]

  let s2 = GenericSubscriptTwo<Double>()
  _ = s2[3.0, 4.0]
  _ = s2[(3.0, 4.0)] // expected-error {{missing argument for parameter #2 in call}}

  let s3 = GenericSubscriptTuple<Double>()
  _ = s3[3.0, 4.0] // expected-error {{extra argument in call}}
  _ = s3[(3.0, 4.0)]

  let s3a = GenericSubscriptLabeledTuple<Double>()
  _ = s3a[x: 3.0, 4.0] // expected-error {{extra argument in call}}
  _ = s3a[x: (3.0, 4.0)]
}

do {
  let a = 3.0
  let b = 4.0
  let d = (a, b)

  let s1 = GenericSubscript<(Double, Double)>()
  _ = s1[a, b]
  _ = s1[(a, b)]
  _ = s1[d]

  let s2 = GenericSubscriptTwo<Double>()
  _ = s2[a, b]
  _ = s2[(a, b)] // Does not diagnose in Swift 3 mode
  _ = s2[d] // Does not diagnose in Swift 3 mode

  let s3 = GenericSubscriptTuple<Double>()
  _ = s3[a, b] // Does not diagnose in Swift 3 mode
  _ = s3[(a, b)]
  _ = s3[d]
}

do {
  var a = 3.0 // expected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4.0 // expected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var d = (a, b) // expected-warning {{variable 'd' was never mutated; consider changing to 'let' constant}}

  var s1 = GenericSubscript<(Double, Double)>()
  _ = s1[a, b]
  _ = s1[(a, b)] // expected-error {{expression type '@lvalue Int' is ambiguous without more context}}
  _ = s1[d] // expected-error {{expression type '@lvalue Int' is ambiguous without more context}}

  var s2 = GenericSubscriptTwo<Double>()
  _ = s2[a, b]
  _ = s2[(a, b)] // expected-error {{cannot convert value of type '(Double, Double)' to expected argument type '(_, _)'}}
  _ = s2[d] // expected-error {{cannot convert value of type '(Double, Double)' to expected argument type '(_, _)'}}

  var s3 = GenericSubscriptTuple<Double>()
  _ = s3[a, b] // expected-error {{extra argument in call}}
  _ = s3[(a, b)]
  _ = s3[d]
}

enum GenericEnum<T> {
  case one(T)
  case labeled(x: T)
  case two(T, T) // expected-note 8 {{'two' declared here}}
  case tuple((T, T))
}

do {
  _ = GenericEnum.one(3, 4)
  _ = GenericEnum.one((3, 4))

  _ = GenericEnum.labeled(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.labeled(x: (3, 4))
  _ = GenericEnum.labeled(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.labeled((3, 4)) // expected-error {{missing argument label 'x:' in call}}

  _ = GenericEnum.two(3, 4)
  _ = GenericEnum.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.tuple((3, 4))
}

do {
  _ = GenericEnum<(Int, Int)>.one(3, 4)
  _ = GenericEnum<(Int, Int)>.one((3, 4)) // Does not diagnose in Swift 3 mode

  _ = GenericEnum<(Int, Int)>.labeled(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum<(Int, Int)>.labeled(x: (3, 4))
  _ = GenericEnum<(Int, Int)>.labeled(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum<(Int, Int)>.labeled((3, 4)) // expected-error {{missing argument label 'x:' in call}}

  _ = GenericEnum<Int>.two(3, 4)
  _ = GenericEnum<Int>.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum<Int>.tuple(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum<Int>.tuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericEnum.one(a, b)
  _ = GenericEnum.one((a, b))
  _ = GenericEnum.one(c)

  _ = GenericEnum.two(a, b)
  _ = GenericEnum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(a, b) // expected-error {{extra argument in call}}
  _ = GenericEnum.tuple((a, b))
  _ = GenericEnum.tuple(c)
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericEnum<(Int, Int)>.one(a, b)
  _ = GenericEnum<(Int, Int)>.one((a, b))
  _ = GenericEnum<(Int, Int)>.one(c)

  _ = GenericEnum<Int>.two(a, b)
  _ = GenericEnum<Int>.two((a, b))
  _ = GenericEnum<Int>.two(c) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}

  _ = GenericEnum<Int>.tuple(a, b)
  _ = GenericEnum<Int>.tuple((a, b))
  _ = GenericEnum<Int>.tuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  // _ = GenericEnum.one(a, b) // Crashes in actual Swift 3
  _ = GenericEnum.one((a, b)) // Does not diagnose in Swift 3 mode
  _ = GenericEnum.one(c) // Does not diagnose in Swift 3 mode

  _ = GenericEnum.two(a, b)
  _ = GenericEnum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(a, b) // expected-error {{extra argument in call}}
  _ = GenericEnum.tuple((a, b))
  _ = GenericEnum.tuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  // _ = GenericEnum<(Int, Int)>.one(a, b) // Crashes in actual Swift 3
  _ = GenericEnum<(Int, Int)>.one((a, b)) // Does not diagnose in Swift 3 mode
  _ = GenericEnum<(Int, Int)>.one(c) // Does not diagnose in Swift 3 mode

  _ = GenericEnum<Int>.two(a, b)
  _ = GenericEnum<Int>.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum<Int>.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum<Int>.tuple(a, b) // expected-error {{extra argument in call}}
  _ = GenericEnum<Int>.tuple((a, b))
  _ = GenericEnum<Int>.tuple(c)
}

protocol Protocol {
  associatedtype Element
}

extension Protocol {
  func requirement(_ x: Element) {}
  func requirementLabeled(x: Element) {}
  func requirementTwo(_ x: Element, _ y: Element) {} // expected-note 2 {{'requirementTwo' declared here}}
  func requirementTuple(_ x: (Element, Element)) {}
}

struct GenericConforms<T> : Protocol {
  typealias Element = T
}

do {
  let s = GenericConforms<Double>()

  s.requirement(3.0)
  s.requirement((3.0))

  s.requirementLabeled(x: 3.0)
  s.requirementLabeled(x: (3.0))

  s.requirementTwo(3.0, 4.0)
  s.requirementTwo((3.0, 4.0)) // expected-error {{missing argument for parameter #2 in call}}

  s.requirementTuple(3.0, 4.0) // expected-error {{extra argument in call}}
  s.requirementTuple((3.0, 4.0))

  let sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.requirement((3.0, 4.0))

  sTwo.requirementLabeled(x: 3.0, 4.0) // expected-error {{extra argument in call}}
  sTwo.requirementLabeled(x: (3.0, 4.0))
}

do {
  let s = GenericConforms<Double>()

  let a = 3.0
  let b = 4.0
  let c = (3.0)
  let d = (a, b)

  s.requirement(a)
  s.requirement((a))
  s.requirement(c)

  s.requirementTwo(a, b)
  s.requirementTwo((a, b)) // Does not diagnose in Swift 3 mode

  s.requirementTuple(a, b) // Does not diagnose in Swift 3 mode
  s.requirementTuple((a, b))

  let sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(a, b)
  sTwo.requirement((a, b))
  sTwo.requirement(d)
}

do {
  var s = GenericConforms<Double>()

  var a = 3.0
  var b = 4.0
  var c = (3.0)
  var d = (a, b)

  s.requirement(a)
  s.requirement((a))
  s.requirement(c)

  s.requirementTwo(a, b)
  s.requirementTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.requirementTuple(a, b) // expected-error {{extra argument in call}}
  s.requirementTuple((a, b))

  var sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(a, b) // expected-error {{extra argument in call}}
  sTwo.requirement((a, b))
  sTwo.requirement(d)
}

extension Protocol {
  func takesClosure(_ fn: (Element) -> ()) {}
  func takesClosureTwo(_ fn: (Element, Element) -> ()) {}
  func takesClosureTuple(_ fn: ((Element, Element)) -> ()) {}
}

do {
  let s = GenericConforms<Double>()
  s.takesClosure({ _ = $0 })
  s.takesClosure({ x in })
  s.takesClosure({ (x: Double) in })

  s.takesClosureTwo({ _ = $0 })
  s.takesClosureTwo({ x in })
  s.takesClosureTwo({ (x: (Double, Double)) in })
  s.takesClosureTwo({ _ = $0; _ = $1 })
  s.takesClosureTwo({ (x, y) in })
  s.takesClosureTwo({ (x: Double, y:Double) in })

  s.takesClosureTuple({ _ = $0 })
  s.takesClosureTuple({ x in })
  s.takesClosureTuple({ (x: (Double, Double)) in })
  s.takesClosureTuple({ _ = $0; _ = $1 })
  s.takesClosureTuple({ (x, y) in })
  s.takesClosureTuple({ (x: Double, y:Double) in })

  let sTwo = GenericConforms<(Double, Double)>()
  sTwo.takesClosure({ _ = $0 })
  sTwo.takesClosure({ x in })
  sTwo.takesClosure({ (x: (Double, Double)) in })
  sTwo.takesClosure({ _ = $0; _ = $1 })
  sTwo.takesClosure({ (x, y) in })
  sTwo.takesClosure({ (x: Double, y: Double) in })
}

do {
  let _: ((Int, Int)) -> () = { _ = $0 }
  let _: ((Int, Int)) -> () = { _ = ($0.0, $0.1) }
  let _: ((Int, Int)) -> () = { t in _ = (t.0, t.1) }

  let _: ((Int, Int)) -> () = { _ = ($0, $1) }
  let _: ((Int, Int)) -> () = { t, u in _ = (t, u) }

  let _: (Int, Int) -> () = { _ = $0 }
  let _: (Int, Int) -> () = { _ = ($0.0, $0.1) }
  let _: (Int, Int) -> () = { t in _ = (t.0, t.1) }

  let _: (Int, Int) -> () = { _ = ($0, $1) }
  let _: (Int, Int) -> () = { t, u in _ = (t, u) }
}

// rdar://problem/28952837 - argument labels ignored when calling function
// with single 'Any' parameter
func takesAny(_: Any) {}

enum HasAnyCase {
  case any(_: Any)
}

do {
  let fn: (Any) -> () = { _ in }

  fn(123)
  fn(data: 123)

  takesAny(123)
  takesAny(data: 123)

  _ = HasAnyCase.any(123)
  _ = HasAnyCase.any(data: 123)
}

// rdar://problem/29739905 - protocol extension methods on Array had
// ParenType sugar stripped off the element type
typealias BoolPair = (Bool, Bool)

func processArrayOfFunctions(f1: [((Bool, Bool)) -> ()],
                             f2: [(Bool, Bool) -> ()],
                             c: Bool) {
  let p = (c, c)

  f1.forEach { block in
    block(p)
    block((c, c))
    block(c, c)
  }

  f2.forEach { block in
    block(p) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}
    block((c, c))
    block(c, c)
  }

  f2.forEach { (block: ((Bool, Bool)) -> ()) in
    block(p)
    block((c, c))
    block(c, c)
  }

  f2.forEach { (block: (Bool, Bool) -> ()) in
    block(p) // expected-error {{passing 2 arguments to a callee as a single tuple value has been removed in Swift 3}}
    block((c, c))
    block(c, c)
  }
}

// expected-error@+1 {{cannot create a single-element tuple with an element label}}
func singleElementTupleArgument(completion: ((didAdjust: Bool)) -> Void) {
    // TODO: Error could be improved.
    // expected-error@+1 {{cannot convert value of type '(didAdjust: Bool)' to expected argument type 'Bool'}}
    completion((didAdjust: true))
}

