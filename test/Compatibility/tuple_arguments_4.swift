// RUN: %target-typecheck-verify-swift -swift-version 4

// See test/Compatibility/tuple_arguments_3.swift for the Swift 3 behavior.

func concrete(_ x: Int) {}
func concreteLabeled(x: Int) {}
func concreteTwo(_ x: Int, _ y: Int) {} // expected-note 5 {{'concreteTwo' declared here}}
func concreteTuple(_ x: (Int, Int)) {}

do {
  concrete(3)
  concrete((3))

  concreteLabeled(x: 3)
  concreteLabeled(x: (3))
  concreteLabeled((x: 3)) // expected-error {{missing argument label 'x:' in call}}

  concreteTwo(3, 4)
  concreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  concreteTuple(3, 4) // expected-error {{global function 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
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
  concreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  concreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  concreteTuple(a, b) // expected-error {{global function 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  concreteTuple((a, b))
  concreteTuple(d)
}

do {
  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  concrete(a)
  concrete((a))
  concrete(c)

  concreteTwo(a, b)
  concreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  concreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  concreteTuple(a, b) // expected-error {{global function 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  concreteTuple((a, b))
  concreteTuple(d)
}

func generic<T>(_ x: T) {}
func genericLabeled<T>(x: T) {}
func genericTwo<T, U>(_ x: T, _ y: U) {} // expected-note 5 {{'genericTwo' declared here}}
func genericTuple<T, U>(_ x: (T, U)) {}

do {
  generic(3)
  generic(3, 4) // expected-error {{extra argument in call}}
  generic((3))
  generic((3, 4))

  genericLabeled(x: 3)
  genericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  genericLabeled(x: (3))
  genericLabeled(x: (3, 4))

  genericTwo(3, 4)
  genericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(3, 4) // expected-error {{global function 'genericTuple' expects a single parameter of type '(T, U)'}} {{16-16=(}} {{20-20=)}}
  genericTuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  generic(a)
  generic(a, b) // expected-error {{extra argument in call}}
  generic((a))
  generic(c)
  generic((a, b))
  generic(d)

  genericTwo(a, b)
  genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(a, b) // expected-error {{global function 'genericTuple' expects a single parameter of type '(T, U)'}} {{16-16=(}} {{20-20=)}}
  genericTuple((a, b))
  genericTuple(d)
}

do {
  var a = 3
  var b = 4
  var c = (3)
  var d = (a, b)

  generic(a)
  generic(a, b) // expected-error {{extra argument in call}}
  generic((a))
  generic(c)
  generic((a, b))
  generic(d)

  genericTwo(a, b)
  genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  genericTuple(a, b) // expected-error {{global function 'genericTuple' expects a single parameter of type '(T, U)'}} {{16-16=(}} {{20-20=)}}
  genericTuple((a, b))
  genericTuple(d)
}

var function: (Int) -> ()
var functionTwo: (Int, Int) -> () // expected-note 5 {{'functionTwo' declared here}}
var functionTuple: ((Int, Int)) -> ()

do {
  function(3)
  function((3))

  functionTwo(3, 4)
  functionTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  functionTuple(3, 4) // expected-error {{var 'functionTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
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
  functionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  functionTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  functionTuple(a, b) // expected-error {{var 'functionTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
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

  functionTuple(a, b) // expected-error {{var 'functionTuple' expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  functionTuple((a, b))
  functionTuple(d)
}

struct Concrete {}

extension Concrete {
  func concrete(_ x: Int) {}
  func concreteTwo(_ x: Int, _ y: Int) {} // expected-note 5 {{'concreteTwo' declared here}}
  func concreteTuple(_ x: (Int, Int)) {}
}

do {
  let s = Concrete()

  s.concrete(3)
  s.concrete((3))

  s.concreteTwo(3, 4)
  s.concreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.concreteTuple(3, 4) // expected-error {{instance method 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{19-19=(}} {{23-23=)}}
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
  s.concreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.concreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.concreteTuple(a, b) // expected-error {{instance method 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{19-19=(}} {{23-23=)}}
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

  s.concreteTuple(a, b) // expected-error {{instance method 'concreteTuple' expects a single parameter of type '(Int, Int)'}} {{19-19=(}} {{23-23=)}}
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
  s.generic(3, 4) // expected-error {{extra argument in call}}
  s.generic((3))
  s.generic((3, 4))

  s.genericLabeled(x: 3)
  s.genericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  s.genericLabeled(x: (3))
  s.genericLabeled(x: (3, 4))

  s.genericTwo(3, 4)
  s.genericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(3, 4) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(T, U)'}} {{18-18=(}} {{22-22=)}}
  s.genericTuple((3, 4))
}

do {
  let s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.generic(a)
  s.generic(a, b) // expected-error {{extra argument in call}}
  s.generic((a))
  s.generic((a, b))
  s.generic(d)

  s.genericTwo(a, b)
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(T, U)'}} {{18-18=(}} {{22-22=)}}
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
  s.generic(a, b) // expected-error {{extra argument in call}}
  s.generic((a))
  s.generic((a, b))
  s.generic(d)

  s.genericTwo(a, b)
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.genericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(T, U)'}} {{18-18=(}} {{22-22=)}}
  s.genericTuple((a, b))
  s.genericTuple(d)
}

extension Concrete {
  mutating func mutatingConcrete(_ x: Int) {}
  mutating func mutatingConcreteTwo(_ x: Int, _ y: Int) {} // expected-note 5 {{'mutatingConcreteTwo' declared here}}
  mutating func mutatingConcreteTuple(_ x: (Int, Int)) {}
}

do {
  var s = Concrete()

  s.mutatingConcrete(3)
  s.mutatingConcrete((3))

  s.mutatingConcreteTwo(3, 4)
  s.mutatingConcreteTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingConcreteTuple(3, 4) // expected-error {{instance method 'mutatingConcreteTuple' expects a single parameter of type '(Int, Int)'}} {{27-27=(}} {{31-31=)}}
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
  s.mutatingConcreteTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingConcreteTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingConcreteTuple(a, b) // expected-error {{instance method 'mutatingConcreteTuple' expects a single parameter of type '(Int, Int)'}} {{27-27=(}} {{31-31=)}}
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

  s.mutatingConcreteTuple(a, b) // expected-error {{instance method 'mutatingConcreteTuple' expects a single parameter of type '(Int, Int)'}} {{27-27=(}} {{31-31=)}}
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
  s.mutatingGeneric(3, 4) // expected-error {{extra argument in call}}
  s.mutatingGeneric((3))
  s.mutatingGeneric((3, 4))

  s.mutatingGenericLabeled(x: 3)
  s.mutatingGenericLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  s.mutatingGenericLabeled(x: (3))
  s.mutatingGenericLabeled(x: (3, 4))

  s.mutatingGenericTwo(3, 4)
  s.mutatingGenericTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(3, 4) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(T, U)'}} {{26-26=(}} {{30-30=)}}
  s.mutatingGenericTuple((3, 4))
}

do {
  var s = Concrete()

  let a = 3
  let b = 4
  let c = (3)
  let d = (a, b)

  s.mutatingGeneric(a)
  s.mutatingGeneric(a, b) // expected-error {{extra argument in call}}
  s.mutatingGeneric((a))
  s.mutatingGeneric((a, b))
  s.mutatingGeneric(d)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingGenericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(T, U)'}} {{26-26=(}} {{30-30=)}}
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
  s.mutatingGeneric(a, b) // expected-error {{extra argument in call}}
  s.mutatingGeneric((a))
  s.mutatingGeneric((a, b))
  s.mutatingGeneric(d)

  s.mutatingGenericTwo(a, b)
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.mutatingGenericTwo(d) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(T, U)'}} {{26-26=(}} {{30-30=)}}
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

  s.functionTuple(3, 4) // expected-error {{single parameter of type '(Int, Int)' is expected in call}} {{19-19=(}} {{23-23=)}}
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
  s.functionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  s.functionTwo(d) // expected-error {{missing argument for parameter #2 in call}}


  s.functionTuple(a, b) // expected-error {{single parameter of type '(Int, Int)' is expected in call}} {{19-19=(}} {{23-23=)}}
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

  s.functionTuple(a, b) // expected-error {{single parameter of type '(Int, Int)' is expected in call}} {{19-19=(}} {{23-23=)}}
  s.functionTuple((a, b))
  s.functionTuple(d)
}

struct InitTwo {
  init(_ x: Int, _ y: Int) {} // expected-note 5 {{'init' declared here}}
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

  _ = InitTuple(3, 4) // expected-error {{initializer expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  _ = InitTuple((3, 4))

  _ = InitLabeledTuple(x: 3, 4) // expected-error {{initializer expects a single parameter of type '(Int, Int)'}}
  _ = InitLabeledTuple(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = InitTwo(a, b)
  _ = InitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = InitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = InitTuple(a, b) // expected-error {{initializer expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  _ = InitTuple((a, b))
  _ = InitTuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = InitTwo(a, b)
  _ = InitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = InitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = InitTuple(a, b) // expected-error {{initializer expects a single parameter of type '(Int, Int)'}} {{17-17=(}} {{21-21=)}}
  _ = InitTuple((a, b))
  _ = InitTuple(c)
}

struct SubscriptTwo {
  subscript(_ x: Int, _ y: Int) -> Int { get { return 0 } set { } } // expected-note 5 {{'subscript' declared here}}
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
  _ = s2[3, 4] // expected-error {{subscript expects a single parameter of type '(Int, Int)'}} {{10-10=(}} {{14-14=)}}
  _ = s2[(3, 4)]
}

do {
  let a = 3
  let b = 4
  let d = (a, b)

  let s1 = SubscriptTwo()
  _ = s1[a, b]
  _ = s1[(a, b)] // expected-error {{missing argument for parameter #2 in call}}
  _ = s1[d] // expected-error {{missing argument for parameter #2 in call}}

  let s2 = SubscriptTuple()
  _ = s2[a, b] // expected-error {{subscript expects a single parameter of type '(Int, Int)'}} {{10-10=(}} {{14-14=)}}
  _ = s2[(a, b)]
  _ = s2[d]

  let s3 = SubscriptLabeledTuple()
  _ = s3[x: 3, 4] // expected-error {{subscript expects a single parameter of type '(Int, Int)'}}
  _ = s3[x: (3, 4)]
}

do {
  // TODO: Restore regressed diagnostics rdar://problem/31724211
  var a = 3 // e/xpected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4 // e/xpected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var d = (a, b) // e/xpected-warning {{variable 'd' was never mutated; consider changing to 'let' constant}}

  var s1 = SubscriptTwo()
  _ = s1[a, b]
  _ = s1[(a, b)] // expected-error {{missing argument for parameter #2 in call}}
  _ = s1[d] // expected-error {{missing argument for parameter #2 in call}}

  var s2 = SubscriptTuple()
  _ = s2[a, b] // expected-error {{subscript expects a single parameter of type '(Int, Int)'}} {{10-10=(}} {{14-14=)}}
  _ = s2[(a, b)]
  _ = s2[d]
}

enum Enum {
  case two(Int, Int) // expected-note 6 {{'two' declared here}}
  case tuple((Int, Int))
  case labeledTuple(x: (Int, Int))
}

do {
  _ = Enum.two(3, 4)
  _ = Enum.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}
  _ = Enum.two(3 > 4 ? 3 : 4) // expected-error {{missing argument for parameter #2 in call}}

  _ = Enum.tuple(3, 4) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{18-18=(}} {{22-22=)}}
  _ = Enum.tuple((3, 4))

  _ = Enum.labeledTuple(x: 3, 4) // expected-error {{enum case 'labeledTuple' expects a single parameter of type '(Int, Int)'}}
  _ = Enum.labeledTuple(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = Enum.two(a, b)
  _ = Enum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = Enum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = Enum.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{18-18=(}} {{22-22=)}}
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

  _ = Enum.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{18-18=(}} {{22-22=)}}
  _ = Enum.tuple((a, b))
  _ = Enum.tuple(c)
}

struct Generic<T> {}

extension Generic {
  func generic(_ x: T) {}
  func genericLabeled(x: T) {}
  func genericTwo(_ x: T, _ y: T) {} // expected-note 3 {{'genericTwo' declared here}}
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

  s.genericTuple(3.0, 4.0) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(Double, Double)'}} {{18-18=(}} {{26-26=)}}
  s.genericTuple((3.0, 4.0))

  let sTwo = Generic<(Double, Double)>()

  sTwo.generic(3.0, 4.0) // expected-error {{instance method 'generic' expects a single parameter of type '(Double, Double)'}} {{16-16=(}} {{24-24=)}}
  sTwo.generic((3.0, 4.0))

  sTwo.genericLabeled(x: 3.0, 4.0) // expected-error {{instance method 'genericLabeled' expects a single parameter of type '(Double, Double)'}}
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
  s.genericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericTuple(a, b) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(Double, Double)'}} {{18-18=(}} {{22-22=)}}
  s.genericTuple((a, b))

  let sTwo = Generic<(Double, Double)>()

  sTwo.generic(a, b) // expected-error {{instance method 'generic' expects a single parameter of type '(Double, Double)'}} {{16-16=(}} {{20-20=)}}
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

  s.genericTuple(a, b) // expected-error {{instance method 'genericTuple' expects a single parameter of type '(Double, Double)'}} {{18-18=(}} {{22-22=)}}
  s.genericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.generic(a, b) // expected-error {{instance method 'generic' expects a single parameter of type '(Double, Double)'}} {{16-16=(}} {{20-20=)}}
  sTwo.generic((a, b))
  sTwo.generic(d)
}

extension Generic {
  mutating func mutatingGeneric(_ x: T) {}
  mutating func mutatingGenericLabeled(x: T) {}
  mutating func mutatingGenericTwo(_ x: T, _ y: T) {} // expected-note 3 {{'mutatingGenericTwo' declared here}}
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

  s.mutatingGenericTuple(3.0, 4.0) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(Double, Double)'}} {{26-26=(}} {{34-34=)}}
  s.mutatingGenericTuple((3.0, 4.0))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(3.0, 4.0) // expected-error {{instance method 'mutatingGeneric' expects a single parameter of type '(Double, Double)'}} {{24-24=(}} {{32-32=)}}
  sTwo.mutatingGeneric((3.0, 4.0))

  sTwo.mutatingGenericLabeled(x: 3.0, 4.0) // expected-error {{instance method 'mutatingGenericLabeled' expects a single parameter of type '(Double, Double)'}}
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
  s.mutatingGenericTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.mutatingGenericTuple(a, b) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(Double, Double)'}} {{26-26=(}} {{30-30=)}}
  s.mutatingGenericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(a, b) // expected-error {{instance method 'mutatingGeneric' expects a single parameter of type '(Double, Double)'}} {{24-24=(}} {{28-28=)}}
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

  s.mutatingGenericTuple(a, b) // expected-error {{instance method 'mutatingGenericTuple' expects a single parameter of type '(Double, Double)'}} {{26-26=(}} {{30-30=)}}
  s.mutatingGenericTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.mutatingGeneric(a, b) // expected-error {{instance method 'mutatingGeneric' expects a single parameter of type '(Double, Double)'}} {{24-24=(}} {{28-28=)}}
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

  s.genericFunctionTuple(3.0, 4.0) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{26-26=(}} {{34-34=)}}
  s.genericFunctionTuple((3.0, 4.0))

  let sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(3.0, 4.0) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{24-24=(}} {{32-32=)}}
  sTwo.genericFunction((3.0, 4.0))
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
  s.genericFunctionTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.genericFunctionTuple(a, b) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{26-26=(}} {{30-30=)}}
  s.genericFunctionTuple((a, b))

  let sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(a, b) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{24-24=(}} {{28-28=)}}
  sTwo.genericFunction((a, b))
  sTwo.genericFunction(d)
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

  s.genericFunctionTuple(a, b) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{26-26=(}} {{30-30=)}}
  s.genericFunctionTuple((a, b))

  var sTwo = Generic<(Double, Double)>()

  sTwo.genericFunction(a, b) // expected-error {{single parameter of type '(Double, Double)' is expected in call}} {{24-24=(}} {{28-28=)}}
  sTwo.genericFunction((a, b))
  sTwo.genericFunction(d)
}

struct GenericInit<T> {
  init(_ x: T) {}
}

struct GenericInitLabeled<T> {
  init(x: T) {}
}

struct GenericInitTwo<T> {
  init(_ x: T, _ y: T) {} // expected-note 10 {{'init' declared here}}
}

struct GenericInitTuple<T> {
  init(_ x: (T, T)) {}
}

struct GenericInitLabeledTuple<T> {
  init(x: (T, T)) {}
}

do {
  _ = GenericInit(3, 4) // expected-error {{extra argument in call}}
  _ = GenericInit((3, 4))

  _ = GenericInitLabeled(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeled(x: (3, 4))

  _ = GenericInitTwo(3, 4)
  _ = GenericInitTwo((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(3, 4) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{24-24=(}} {{28-28=)}}
  _ = GenericInitTuple((3, 4))

  _ = GenericInitLabeledTuple(x: 3, 4) // expected-error {{initializer expects a single parameter of type '(T, T)'}}
  _ = GenericInitLabeledTuple(x: (3, 4))
}

do {
  _ = GenericInit<(Int, Int)>(3, 4) // expected-error {{extra argument in call}}
  _ = GenericInit<(Int, Int)>((3, 4))

  _ = GenericInitLabeled<(Int, Int)>(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericInitLabeled<(Int, Int)>(x: (3, 4))

  _ = GenericInitTwo<Int>(3, 4)
  _ = GenericInitTwo<Int>((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple<Int>(3, 4) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{29-29=(}} {{33-33=)}}
  _ = GenericInitTuple<Int>((3, 4))

  _ = GenericInitLabeledTuple<Int>(x: 3, 4) // expected-error {{initializer expects a single parameter of type '(T, T)'}}
  _ = GenericInitLabeledTuple<Int>(x: (3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericInit(a, b) // expected-error {{extra argument in call}}
  _ = GenericInit((a, b))
  _ = GenericInit(c)

  _ = GenericInitTwo(a, b)
  _ = GenericInitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(a, b) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{24-24=(}} {{28-28=)}}
  _ = GenericInitTuple((a, b))
  _ = GenericInitTuple(c)
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericInit<(Int, Int)>(a, b) // expected-error {{extra argument in call}}
  _ = GenericInit<(Int, Int)>((a, b))
  _ = GenericInit<(Int, Int)>(c)

  _ = GenericInitTwo<Int>(a, b)
  _ = GenericInitTwo<Int>((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo<Int>(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple<Int>(a, b) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{29-29=(}} {{33-33=)}}
  _ = GenericInitTuple<Int>((a, b))
  _ = GenericInitTuple<Int>(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = GenericInit(a, b) // expected-error {{extra argument in call}}
  _ = GenericInit((a, b))
  _ = GenericInit(c)

  _ = GenericInitTwo(a, b)
  _ = GenericInitTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple(a, b) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{24-24=(}} {{28-28=)}}
  _ = GenericInitTuple((a, b))
  _ = GenericInitTuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = GenericInit<(Int, Int)>(a, b) // expected-error {{extra argument in call}}
  _ = GenericInit<(Int, Int)>((a, b))
   _ = GenericInit<(Int, Int)>(c)

  _ = GenericInitTwo<Int>(a, b)
  _ = GenericInitTwo<Int>((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericInitTwo<Int>(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericInitTuple<Int>(a, b) // expected-error {{initializer expects a single parameter of type '(T, T)'}} {{29-29=(}} {{33-33=)}}
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
  subscript(_ x: T, _ y: T) -> Int { get { return 0 } set { } } // expected-note 5 {{'subscript' declared here}}
}

struct GenericSubscriptLabeledTuple<T> {
  subscript(x x: (T, T)) -> Int { get { return 0 } set { } }
}

struct GenericSubscriptTuple<T> {
  subscript(_ x: (T, T)) -> Int { get { return 0 } set { } }
}

do {
  let s1 = GenericSubscript<(Double, Double)>()
  _ = s1[3.0, 4.0] // expected-error {{extra argument in call}}
  _ = s1[(3.0, 4.0)]

  let s1a  = GenericSubscriptLabeled<(Double, Double)>()
  _ = s1a [x: 3.0, 4.0] // expected-error {{extra argument in call}}
  _ = s1a [x: (3.0, 4.0)]

  let s2 = GenericSubscriptTwo<Double>()
  _ = s2[3.0, 4.0]
  _ = s2[(3.0, 4.0)] // expected-error {{missing argument for parameter #2 in call}}

  let s3 = GenericSubscriptTuple<Double>()
  _ = s3[3.0, 4.0] // expected-error {{subscript expects a single parameter of type '(T, T)'}} {{10-10=(}} {{18-18=)}}
  _ = s3[(3.0, 4.0)]

  let s3a = GenericSubscriptLabeledTuple<Double>()
  _ = s3a[x: 3.0, 4.0] // expected-error {{subscript expects a single parameter of type '(T, T)'}}
  _ = s3a[x: (3.0, 4.0)]
}

do {
  let a = 3.0
  let b = 4.0
  let d = (a, b)

  let s1 = GenericSubscript<(Double, Double)>()
  _ = s1[a, b] // expected-error {{extra argument in call}}
  _ = s1[(a, b)]
  _ = s1[d]

  let s2 = GenericSubscriptTwo<Double>()
  _ = s2[a, b]
  _ = s2[(a, b)] // expected-error {{missing argument for parameter #2 in call}}
  _ = s2[d] // expected-error {{missing argument for parameter #2 in call}}

  let s3 = GenericSubscriptTuple<Double>()
  _ = s3[a, b] // expected-error {{subscript expects a single parameter of type '(T, T)'}} {{10-10=(}} {{14-14=)}}
  _ = s3[(a, b)]
  _ = s3[d]
}

do {
  // TODO: Restore regressed diagnostics rdar://problem/31724211
  var a = 3.0 // e/xpected-warning {{variable 'a' was never mutated; consider changing to 'let' constant}}
  var b = 4.0 // e/xpected-warning {{variable 'b' was never mutated; consider changing to 'let' constant}}
  var d = (a, b) // e/xpected-warning {{variable 'd' was never mutated; consider changing to 'let' constant}}

  var s1 = GenericSubscript<(Double, Double)>()
  _ = s1[a, b] // expected-error {{extra argument in call}}
  _ = s1[(a, b)]
  _ = s1[d]

  var s2 = GenericSubscriptTwo<Double>()
  _ = s2[a, b]
  _ = s2[(a, b)] // expected-error {{missing argument for parameter #2 in call}}
  _ = s2[d] // expected-error {{missing argument for parameter #2 in call}}

  var s3 = GenericSubscriptTuple<Double>()
  _ = s3[a, b] // expected-error {{subscript expects a single parameter of type '(T, T)'}} {{10-10=(}} {{14-14=)}}
  _ = s3[(a, b)]
  _ = s3[d]
}

enum GenericEnum<T> {
  case one(T)
  case labeled(x: T)
  case two(T, T) // expected-note 10 {{'two' declared here}}
  case tuple((T, T))
}

do {
  _ = GenericEnum.one(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.one((3, 4))

  _ = GenericEnum.labeled(x: 3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.labeled(x: (3, 4))
  _ = GenericEnum.labeled(3, 4) // expected-error {{extra argument in call}}
  _ = GenericEnum.labeled((3, 4)) // expected-error {{missing argument label 'x:' in call}}

  _ = GenericEnum.two(3, 4)
  _ = GenericEnum.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(3, 4) // expected-error {{enum case 'tuple' expects a single parameter of type '(T, T)'}} {{25-25=(}} {{29-29=)}}
  _ = GenericEnum.tuple((3, 4))
}

do {
  _ = GenericEnum<(Int, Int)>.one(3, 4) // expected-error {{enum case 'one' expects a single parameter of type '(Int, Int)'}} {{35-35=(}} {{39-39=)}}
  _ = GenericEnum<(Int, Int)>.one((3, 4))

  _ = GenericEnum<(Int, Int)>.labeled(x: 3, 4) // expected-error {{enum case 'labeled' expects a single parameter of type '(Int, Int)'}}
  _ = GenericEnum<(Int, Int)>.labeled(x: (3, 4))
  _ = GenericEnum<(Int, Int)>.labeled(3, 4) // expected-error {{enum case 'labeled' expects a single parameter of type '(Int, Int)'}}
  _ = GenericEnum<(Int, Int)>.labeled((3, 4)) // expected-error {{missing argument label 'x:' in call}}

  _ = GenericEnum<Int>.two(3, 4)
  _ = GenericEnum<Int>.two((3, 4)) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum<Int>.tuple(3, 4) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{30-30=(}} {{34-34=)}}
  _ = GenericEnum<Int>.tuple((3, 4))
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericEnum.one(a, b) // expected-error {{extra argument in call}}
  _ = GenericEnum.one((a, b))
  _ = GenericEnum.one(c)

  _ = GenericEnum.two(a, b)
  _ = GenericEnum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(T, T)'}} {{25-25=(}} {{29-29=)}}
  _ = GenericEnum.tuple((a, b))
  _ = GenericEnum.tuple(c)
}

do {
  let a = 3
  let b = 4
  let c = (a, b)

  _ = GenericEnum<(Int, Int)>.one(a, b) // expected-error {{enum case 'one' expects a single parameter of type '(Int, Int)'}} {{35-35=(}} {{39-39=)}}
  _ = GenericEnum<(Int, Int)>.one((a, b))
  _ = GenericEnum<(Int, Int)>.one(c)

  _ = GenericEnum<Int>.two(a, b)
  _ = GenericEnum<Int>.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum<Int>.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum<Int>.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{30-30=(}} {{34-34=)}}
  _ = GenericEnum<Int>.tuple((a, b))
  _ = GenericEnum<Int>.tuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = GenericEnum.one(a, b) // expected-error {{extra argument in call}}
  _ = GenericEnum.one((a, b))
  _ = GenericEnum.one(c)

  _ = GenericEnum.two(a, b)
  _ = GenericEnum.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(T, T)'}} {{25-25=(}} {{29-29=)}}
  _ = GenericEnum.tuple((a, b))
  _ = GenericEnum.tuple(c)
}

do {
  var a = 3
  var b = 4
  var c = (a, b)

  _ = GenericEnum<(Int, Int)>.one(a, b) // expected-error {{enum case 'one' expects a single parameter of type '(Int, Int)'}} {{35-35=(}} {{39-39=)}}
  _ = GenericEnum<(Int, Int)>.one((a, b))
  _ = GenericEnum<(Int, Int)>.one(c)

  _ = GenericEnum<Int>.two(a, b)
  _ = GenericEnum<Int>.two((a, b)) // expected-error {{missing argument for parameter #2 in call}}
  _ = GenericEnum<Int>.two(c) // expected-error {{missing argument for parameter #2 in call}}

  _ = GenericEnum<Int>.tuple(a, b) // expected-error {{enum case 'tuple' expects a single parameter of type '(Int, Int)'}} {{30-30=(}} {{34-34=)}}
  _ = GenericEnum<Int>.tuple((a, b))
  _ = GenericEnum<Int>.tuple(c)
}

protocol Protocol {
  associatedtype Element
}

extension Protocol {
  func requirement(_ x: Element) {}
  func requirementLabeled(x: Element) {}
  func requirementTwo(_ x: Element, _ y: Element) {} // expected-note 3 {{'requirementTwo' declared here}}
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

  s.requirementTuple(3.0, 4.0) // expected-error {{instance method 'requirementTuple' expects a single parameter of type '(Double, Double)'}} {{22-22=(}} {{30-30=)}}
  s.requirementTuple((3.0, 4.0))

  let sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(3.0, 4.0) // expected-error {{instance method 'requirement' expects a single parameter of type '(Double, Double)'}} {{20-20=(}} {{28-28=)}}
  sTwo.requirement((3.0, 4.0))

  sTwo.requirementLabeled(x: 3.0, 4.0) // expected-error {{instance method 'requirementLabeled' expects a single parameter of type '(Double, Double)'}}
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
  s.requirementTwo((a, b)) // expected-error {{missing argument for parameter #2 in call}}

  s.requirementTuple(a, b) // expected-error {{instance method 'requirementTuple' expects a single parameter of type '(Double, Double)'}} {{22-22=(}} {{26-26=)}}
  s.requirementTuple((a, b))

  let sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(a, b) // expected-error {{instance method 'requirement' expects a single parameter of type '(Double, Double)'}} {{20-20=(}} {{24-24=)}}
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

  s.requirementTuple(a, b) // expected-error {{instance method 'requirementTuple' expects a single parameter of type '(Double, Double)'}} {{22-22=(}} {{26-26=)}}
  s.requirementTuple((a, b))

  var sTwo = GenericConforms<(Double, Double)>()

  sTwo.requirement(a, b) // expected-error {{instance method 'requirement' expects a single parameter of type '(Double, Double)'}} {{20-20=(}} {{24-24=)}}
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

  s.takesClosureTwo({ _ = $0 }) // expected-error {{contextual closure type '(Double, Double) -> ()' expects 2 arguments, but 1 was used in closure body}}
  s.takesClosureTwo({ x in }) // expected-error {{contextual closure type '(Double, Double) -> ()' expects 2 arguments, but 1 was used in closure body}}
  s.takesClosureTwo({ (x: (Double, Double)) in }) // expected-error {{contextual closure type '(Double, Double) -> ()' expects 2 arguments, but 1 was used in closure body}}
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

  let _: ((Int, Int)) -> () = { _ = ($0, $1) } // expected-error {{closure tuple parameter '(Int, Int)' does not support destructuring}}
  let _: ((Int, Int)) -> () = { t, u in _ = (t, u) } // expected-error {{closure tuple parameter '(Int, Int)' does not support destructuring}} {{33-37=(arg)}} {{41-41=let (t, u) = arg; }}

  let _: (Int, Int) -> () = { _ = $0 } // expected-error {{contextual closure type '(Int, Int) -> ()' expects 2 arguments, but 1 was used in closure body}}
  let _: (Int, Int) -> () = { _ = ($0.0, $0.1) } // expected-error {{contextual closure type '(Int, Int) -> ()' expects 2 arguments, but 1 was used in closure body}}
  let _: (Int, Int) -> () = { t in _ = (t.0, t.1) } // expected-error {{contextual closure type '(Int, Int) -> ()' expects 2 arguments, but 1 was used in closure body}}

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
  fn(data: 123) // expected-error {{extraneous argument label 'data:' in call}}

  takesAny(123)
  takesAny(data: 123) // expected-error {{extraneous argument label 'data:' in call}}

  _ = HasAnyCase.any(123)
  _ = HasAnyCase.any(data: 123) // expected-error {{extraneous argument label 'data:' in call}}
}

// rdar://problem/29739905 - protocol extension methods on Array had
// ParenType sugar stripped off the element type
func processArrayOfFunctions(f1: [((Bool, Bool)) -> ()],
                             f2: [(Bool, Bool) -> ()],
                             c: Bool) {
  let p = (c, c)

  f1.forEach { block in
    block(p)
    block((c, c))
    block(c, c) // expected-error {{parameter 'block' expects a single parameter of type '(Bool, Bool)'}} {{11-11=(}} {{15-15=)}}
  }

  f2.forEach { block in
  // expected-note@-1 2{{'block' declared here}}
    block(p) // expected-error {{missing argument for parameter #2 in call}}
    block((c, c)) // expected-error {{missing argument for parameter #2 in call}}
    block(c, c)
  }

  f2.forEach { (block: ((Bool, Bool)) -> ()) in
  // expected-error@-1 {{cannot convert value of type '(((Bool, Bool)) -> ()) -> ()' to expected argument type '((Bool, Bool) -> ()) -> Void}}
    block(p)
    block((c, c))
    block(c, c)
  }

  f2.forEach { (block: (Bool, Bool) -> ()) in
  // expected-note@-1 2{{'block' declared here}}
    block(p) // expected-error {{missing argument for parameter #2 in call}}
    block((c, c)) // expected-error {{missing argument for parameter #2 in call}}
    block(c, c)
  }
}

// expected-error@+1 {{cannot create a single-element tuple with an element label}}
func singleElementTupleArgument(completion: ((didAdjust: Bool)) -> Void) {
    // TODO: Error could be improved.
    // expected-error@+1 {{cannot convert value of type '(didAdjust: Bool)' to expected argument type 'Bool'}}
    completion((didAdjust: true))
}


// SR-4378

final public class MutableProperty<Value> {
    public init(_ initialValue: Value) {}
}

enum DataSourcePage<T> {
    case notLoaded
}

let pages1: MutableProperty<(data: DataSourcePage<Int>, totalCount: Int)> = MutableProperty((
    data: .notLoaded,
    totalCount: 0
))


let pages2: MutableProperty<(data: DataSourcePage<Int>, totalCount: Int)> = MutableProperty((
    data: DataSourcePage.notLoaded,
    totalCount: 0
))


let pages3: MutableProperty<(data: DataSourcePage<Int>, totalCount: Int)> = MutableProperty((
    data: DataSourcePage<Int>.notLoaded,
    totalCount: 0
))

// SR-4745
let sr4745 = [1, 2]
let _ = sr4745.enumerated().map { (count, element) in "\(count): \(element)" }

// SR-4738

let sr4738 = (1, (2, 3))
[sr4738].map { (x, (y, z)) -> Int in x + y + z } // expected-error {{use of undeclared type 'y'}}
// expected-error@-1 {{closure tuple parameter does not support destructuring}} {{20-26=arg1}} {{38-38=let (y, z) = arg1; }}

// rdar://problem/31892961
let r31892961_1 = [1: 1, 2: 2]
r31892961_1.forEach { (k, v) in print(k + v) }

let r31892961_2 = [1, 2, 3]
let _: [Int] = r31892961_2.enumerated().map { ((index, val)) in
  // expected-error@-1 {{closure tuple parameter does not support destructuring}} {{48-60=arg0}} {{3-3=\n  let (index, val) = arg0\n  }}
  // expected-error@-2 {{use of undeclared type 'index'}}
  val + 1
}

let r31892961_3 = (x: 1, y: 42)
_ = [r31892961_3].map { (x: Int, y: Int) in x + y }

_ = [r31892961_3].map { (x, y: Int) in x + y }

let r31892961_4 = (1, 2)
_ = [r31892961_4].map { x, y in x + y }

let r31892961_5 = (x: 1, (y: 2, (w: 3, z: 4)))
[r31892961_5].map { (x: Int, (y: Int, (w: Int, z: Int))) in x + y }
// expected-error@-1 {{closure tuple parameter does not support destructuring}} {{30-56=arg1}} {{61-61=let (y, (w, z)) = arg1; }}

let r31892961_6 = (x: 1, (y: 2, z: 4))
[r31892961_6].map { (x: Int, (y: Int, z: Int)) in x + y }
// expected-error@-1 {{closure tuple parameter does not support destructuring}} {{30-46=arg1}} {{51-51=let (y, z) = arg1; }}

// rdar://problem/32214649 -- these regressed in Swift 4 mode
// with SE-0110 because of a problem in associated type inference

func r32214649_1<X,Y>(_ a: [X], _ f: (X)->Y) -> [Y] {
  return a.map(f)
}

func r32214649_2<X>(_ a: [X], _ f: (X) -> Bool) -> [X] {
  return a.filter(f)
}

func r32214649_3<X>(_ a: [X]) -> [X] {
  return a.filter { _ in return true }
}

// rdar://problem/32301091 - [SE-0110] causes errors when passing a closure with a single underscore to a block accepting multiple parameters

func rdar32301091_1(_ :((Int, Int) -> ())!) {}
rdar32301091_1 { _ in }
// expected-error@-1 {{cannot convert value of type '(_) -> ()' to expected argument type '((Int, Int) -> ())?'}}

func rdar32301091_2(_ :(Int, Int) -> ()) {}
rdar32301091_2 { _ in }
// expected-error@-1 {{contextual closure type '(Int, Int) -> ()' expects 2 arguments, but 1 was used in closure body}} {{19-19=,_ }}
rdar32301091_2 { x in }
// expected-error@-1 {{contextual closure type '(Int, Int) -> ()' expects 2 arguments, but 1 was used in closure body}} {{19-19=,<#arg#> }}

func rdar32875953() {
  let myDictionary = ["hi":1]

  myDictionary.forEach {
    print("\($0) -> \($1)")
  }

  myDictionary.forEach { key, value in
    print("\(key) -> \(value)")
  }

  myDictionary.forEach { (key, value) in
    print("\(key) -> \(value)")
  }

  let array1 = [1]
  let array2 = [2]

  _ = zip(array1, array2).map(+)
}

struct SR_5199 {}
extension Sequence where Iterator.Element == (key: String, value: String?) {
  func f() -> [SR_5199] {
    return self.map { (key, value) in
        SR_5199() // Ok
    }
  }
}

func rdar33043106(_ records: [(Int)], _ other: [((Int))]) -> [Int] {
  let x: [Int] = records.map { _ in
    let i = 1
    return i
  }
  let y: [Int] = other.map { _ in
    let i = 1
    return i
  }

  return x + y
}

func itsFalse(_: Int) -> Bool? {
  return false
}

func rdar33159366(s: AnySequence<Int>) {
  _ = s.compactMap(itsFalse)
  let a = Array(s)
  _ = a.compactMap(itsFalse)
}

func sr5429<T>(t: T) {
  _ = AnySequence([t]).first(where: { (t: T) in true })
}

extension Concrete {
  typealias T = (Int, Int)
  typealias F = (T) -> ()
  func opt1(_ fn: (((Int, Int)) -> ())?) {}
  func opt2(_ fn: (((Int, Int)) -> ())??) {}
  func opt3(_ fn: (((Int, Int)) -> ())???) {}
  func optAliasT(_ fn: ((T) -> ())?) {}
  func optAliasF(_ fn: F?) {}
}

extension Generic {
  typealias F = (T) -> ()
  func opt1(_ fn: (((Int, Int)) -> ())?) {}
  func opt2(_ fn: (((Int, Int)) -> ())??) {}
  func opt3(_ fn: (((Int, Int)) -> ())???) {}
  func optAliasT(_ fn: ((T) -> ())?) {}
  func optAliasF(_ fn: F?) {}
}

func rdar33239714() {
  Concrete().opt1 { x, y in }
  Concrete().opt1 { (x, y) in }
  Concrete().opt2 { x, y in }
  Concrete().opt2 { (x, y) in }
  Concrete().opt3 { x, y in }
  Concrete().opt3 { (x, y) in }
  Concrete().optAliasT { x, y in }
  Concrete().optAliasT { (x, y) in }
  Concrete().optAliasF { x, y in }
  Concrete().optAliasF { (x, y) in }
  Generic<(Int, Int)>().opt1 { x, y in }
  Generic<(Int, Int)>().opt1 { (x, y) in }
  Generic<(Int, Int)>().opt2 { x, y in }
  Generic<(Int, Int)>().opt2 { (x, y) in }
  Generic<(Int, Int)>().opt3 { x, y in }
  Generic<(Int, Int)>().opt3 { (x, y) in }
  Generic<(Int, Int)>().optAliasT { x, y in }
  Generic<(Int, Int)>().optAliasT { (x, y) in }
  Generic<(Int, Int)>().optAliasF { x, y in }
  Generic<(Int, Int)>().optAliasF { (x, y) in }
}

// rdar://problem/35198459 - source-compat-suite failure: Moya (toType->hasUnresolvedType() && "Should have handled this above"
do {
  func foo(_: (() -> Void)?) {}
  func bar() -> ((()) -> Void)? { return nil }
  foo(bar()) // Allow ((()) -> Void)? to be passed in place of (() -> Void)? for -swift-version 4 but not later.
}

// https://bugs.swift.org/browse/SR-6509
public extension Optional {
  func apply<Result>(_ transform: ((Wrapped) -> Result)?) -> Result? {
    return self.flatMap { value in
      transform.map { $0(value) }
    }
  }

  func apply<Value, Result>(_ value: Value?) -> Result?
    where Wrapped == (Value) -> Result {
    return value.apply(self)
  }
}

// https://bugs.swift.org/browse/SR-6837
do {
  func takeFn(fn: (_ i: Int, _ j: Int?) -> ()) {}
  func takePair(_ pair: (Int, Int?)) {}
  takeFn(fn: takePair) // Allow for -swift-version 4, but not later
  takeFn(fn: { (pair: (Int, Int?)) in } ) // Disallow for -swift-version 4 and later
  // expected-error@-1 {{contextual closure type '(Int, Int?) -> ()' expects 2 arguments, but 1 was used in closure body}}
  takeFn { (pair: (Int, Int?)) in } // Disallow for -swift-version 4 and later
  // expected-error@-1 {{contextual closure type '(Int, Int?) -> ()' expects 2 arguments, but 1 was used in closure body}}
}

// https://bugs.swift.org/browse/SR-6796
do {
  func f(a: (() -> Void)? = nil) {}
  func log<T>() -> ((T) -> Void)? { return nil }

  f(a: log() as ((()) -> Void)?) // Allow ((()) -> Void)? to be passed in place of (() -> Void)? for -swift-version 4 but not later.

  func logNoOptional<T>() -> (T) -> Void { }
  f(a: logNoOptional() as ((()) -> Void)) // Also allow the optional-injected form.

  func g() {}
  g(()) // expected-error {{argument passed to call that takes no arguments}}

  func h(_: ()) {} // expected-note {{'h' declared here}}
  h() // expected-error {{missing argument for parameter #1 in call}}
}


// https://bugs.swift.org/browse/SR-7191
class Mappable<T> {
  init(_: T) { }
  func map<U>(_ body: (T) -> U) -> U { fatalError() }
}

let x = Mappable(())
_ = x.map { (_: Void) in return () }
