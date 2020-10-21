// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE_EXTRAARG | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE_MEMBERS | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=RELATED | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=RELATED_EXTRAARG | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=RELATED_INERROREXPR | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=NOCALLBACK_FALLBACK | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=MULTICLOSURE_FALLBACK | %FileCheck %s --check-prefix=MULTICLOSURE_FALLBACK
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=UNAMBIGUOUSCLOSURE_ARG | %FileCheck %s --check-prefix=UNAMBIGUOUSCLOSURE_ARG
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=AMBIGUOUSCLOSURE_ARG | %FileCheck %s --check-prefix=AMBIGUOUSCLOSURE_ARG
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=AMBIGUOUSCLOSURE_ARG_RETURN | %FileCheck %s --check-prefix=AMBIGUOUSCLOSURE_ARG_RETURN
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_FOO | %FileCheck %s --check-prefix=OVERLOADEDFUNC_FOO
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_BAR | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BAR
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_MISSINGLABEL | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BOTH
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_MISSINGARG_AFTER | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BOTH
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDMEMBER_MISSINGARG_AFTER | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BOTH
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_MISSINGARG_BEFORE | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BAR
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=OVERLOADEDFUNC_MISSINGARG_BEFOREANDAFTER | %FileCheck %s --check-prefix=OVERLOADEDFUNC_BAR
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=ERROR_IN_BASE | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=GENERIC | %FileCheck %s --check-prefix=GENERIC
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=GENERIC_MISSINGARG | %FileCheck %s --check-prefix=NORESULTS
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_MISSINGARG | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_NORETURN | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_FUNCBUILDER | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=MULTICLOSURE_FUNCBUILDER | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=MULTICLOSURE_FUNCBUILDER_ERROR | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=MULTICLOSURE_FUNCBUILDER_FIXME | %FileCheck %s --check-prefix=NORESULTS

struct A {
  func doAThings() -> A { return self }
}

struct B {
  func doBThings() {}
}

func overloadedReturn() -> A { return A() }
func overloadedReturn() -> B { return B() }

overloadedReturn().#^SIMPLE^#
overloadedReturn(1).#^SIMPLE_EXTRAARG^#

struct HasMembers {
  func overloadedReturn() -> A { return A() }
  func overloadedReturn() -> B { return B() }
}

HasMembers().overloadedReturn().#^SIMPLE_MEMBERS^#

func givenErrorExpr(_ a: String) -> A {}
func givenErrorExpr(_ b: Int) -> B {}

givenErrorExpr(undefined).#^ERROR_IN_BASE^#

// SIMPLE: Begin completions, 4 items
// SIMPLE-DAG: Keyword[self]/CurrNominal:          self[#A#]{{; name=.+$}}
// SIMPLE-DAG: Decl[InstanceMethod]/CurrNominal:   doAThings()[#A#]{{; name=.+$}}
// SIMPLE-DAG: Keyword[self]/CurrNominal:          self[#B#]{{; name=.+$}}
// SIMPLE-DAG: Decl[InstanceMethod]/CurrNominal:   doBThings()[#Void#]{{; name=.+$}}
// SIMPLE: End completions

let x: A = overloadedReturn().#^RELATED^#
let x: A = overloadedReturn(1).#^RELATED_EXTRAARG^#

// RELATED: Begin completions, 4 items
// RELATED-DAG: Keyword[self]/CurrNominal:          self[#A#]{{; name=.+$}}
// RELATED-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: doAThings()[#A#]{{; name=.+$}}
// RELATED-DAG: Keyword[self]/CurrNominal:          self[#B#]{{; name=.+$}}
// RELATED-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: doBThings()[#Void#]{{; name=.+$}}
// RELATED: End completions

func takesA(_ callback: () -> A) -> B {}
func takesB(_ item: B) {}

takesB((takesA { return overloadedReturn().#^RELATED_INERROREXPR^# }).)

switch undefined {
  case takesA { return overloadedReturn().#^NOCALLBACK_FALLBACK^# }:
    break
}

func takesClosureA(_ arg: (A) -> ()) {}
func takesClosureB(_ arg: (B) -> ()) {}

takesClosureA { arg in
  takesClosureB { arg in
    arg.#^MULTICLOSURE_FALLBACK^#
  }
  print() + 10
} + 10

// MULTICLOSURE_FALLBACK: Begin completions, 2 items
// MULTICLOSURE_FALLBACK-DAG: Keyword[self]/CurrNominal:        self[#B#]{{; name=.+$}}
// MULTICLOSURE_FALLBACK-DAG: Decl[InstanceMethod]/CurrNominal: doBThings()[#Void#]{{; name=.+$}}
// MULTICLOSURE_FALLBACK: End completions

func takesAnonClosure(_ x: (A) -> A) { return A() }
func takesAnonClosure(_ x: (B, A) -> B { return B() }
func takesAnonClosure(_ x: () -> (A, B) { return (A(), B()) }

struct TestRelations {
  static let a = A()
  static let b = B()
  static let ab = (A(), B())
}

// test we consider both overloads as $0 or $1 may have just not been written yet
takesAnonClosure { $1.#^UNAMBIGUOUSCLOSURE_ARG^# }
// UNAMBIGUOUSCLOSURE_ARG: Begin completions, 2 items
// UNAMBIGUOUSCLOSURE_ARG-DAG: Keyword[self]/CurrNominal: self[#A#]{{; name=.+$}}
// UNAMBIGUOUSCLOSURE_ARG-DAG: Decl[InstanceMethod]/CurrNominal: doAThings()[#A#]{{; name=.+$}}
// UNAMBIGUOUSCLOSURE_ARG: End completions

takesAnonClosure { $0.#^AMBIGUOUSCLOSURE_ARG^# }
// AMBIGUOUSCLOSURE_ARG: Begin completions, 4 items
// AMBIGUOUSCLOSURE_ARG-DAG: Keyword[self]/CurrNominal: self[#A#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: doAThings()[#A#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG-DAG: Keyword[self]/CurrNominal: self[#B#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG-DAG: Decl[InstanceMethod]/CurrNominal: doBThings()[#Void#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG: End completions

takesAnonClosure { TestRelations.#^AMBIGUOUSCLOSURE_ARG_RETURN^# }
// AMBIGUOUSCLOSURE_ARG_RETURN: Begin completions, 6 items
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Keyword[self]/CurrNominal: self[#TestRelations.Type#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Keyword/CurrNominal: Type[#TestRelations.Type#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: a[#A#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: b[#B#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: ab[#(A, B)#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN-DAG: Decl[Constructor]/CurrNominal: init()[#TestRelations#]{{; name=.+$}}
// AMBIGUOUSCLOSURE_ARG_RETURN: End completions


func testMissingArgs() {
  enum Foo { case foo }
  enum Bar { case bar }

  struct Test {
    static let foo = Foo.foo
    static let bar = Bar.bar
  }

  func test(foo: Foo) {}
  func test(bar: Bar) {}

  func test2(first: Bar, second: Int) {}
  func test2(first: Foo) {}

  func test3(skipMe: Int, after: Foo) {}
  func test3(after: Bar) {}

  func test4(skipMe: Int, both: Foo, skipMeToo: Int) {}
  func test4(both: Bar, skipMeTo: Int) {}


  test(foo: Test.#^OVERLOADEDFUNC_FOO^#)
  // OVERLOADEDFUNC_FOO: Begin completions, 5 items
  // OVERLOADEDFUNC_FOO-DAG: Keyword[self]/CurrNominal: self[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_FOO-DAG: Keyword/CurrNominal: Type[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_FOO-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: foo[#Foo#]{{; name=.+$}}
  // OVERLOADEDFUNC_FOO-DAG: Decl[StaticVar]/CurrNominal: bar[#Bar#]{{; name=.+$}}
  // OVERLOADEDFUNC_FOO-DAG: Decl[Constructor]/CurrNominal: init()[#Test#]{{; name=.+$}}
  // OVERLOADEDFUNC_FOO: End completions

  test(bar: Test.#^OVERLOADEDFUNC_BAR^#)
  // OVERLOADEDFUNC_BAR: Begin completions, 5 items
  // OVERLOADEDFUNC_BAR-DAG: Keyword[self]/CurrNominal: self[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_BAR-DAG: Keyword/CurrNominal: Type[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_BAR-DAG: Decl[StaticVar]/CurrNominal: foo[#Foo#]{{; name=.+$}}
  // OVERLOADEDFUNC_BAR-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: bar[#Bar#]{{; name=.+$}}
  // OVERLOADEDFUNC_BAR-DAG: Decl[Constructor]/CurrNominal: init()[#Test#]{{; name=.+$}}
  // OVERLOADEDFUNC_BAR: End completions

  test(Test.#^OVERLOADEDFUNC_MISSINGLABEL^#, extraArg: 2)
  test2(first: Test.#^OVERLOADEDFUNC_MISSINGARG_AFTER^#)

  // Also check ambiguous member functions
  struct TestStruct {
    func test2(first: Bar, second: Int) {}
    func test2(first: Foo) {}
  }

  TestStruct().test2(first: Test.#^OVERLOADEDMEMBER_MISSINGARG_AFTER^#)

  // TODO: Should we insert the missing label in the completion text for OVERLOADEDFUNC_MISSINGLABEL?
  // OVERLOADEDFUNC_BOTH: Begin completions, 5 items
  // OVERLOADEDFUNC_BOTH-DAG: Keyword[self]/CurrNominal: self[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_BOTH-DAG: Keyword/CurrNominal: Type[#Test.Type#]{{; name=.+$}}
  // OVERLOADEDFUNC_BOTH-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: foo[#Foo#]{{; name=.+$}}
  // OVERLOADEDFUNC_BOTH-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: bar[#Bar#]{{; name=.+$}}
  // OVERLOADEDFUNC_BOTH-DAG: Decl[Constructor]/CurrNominal: init()[#Test#]{{; name=.+$}}
  // OVERLOADEDFUNC_BOTH: End completions

  test3(after: Test.#^OVERLOADEDFUNC_MISSINGARG_BEFORE^#);
  test4(both: Test.#^OVERLOADEDFUNC_MISSINGARG_BEFOREANDAFTER^#)
}



protocol C {
  associatedtype Element
  func getCElem() -> Element
}

protocol D {
  associatedtype Item
  func getDElem() -> Item
}

struct CDStruct: C, D {
  func getDElem() -> A { return A() }
  func getCElem() -> B { return B() }
}

func genericReturn<T:C, U>(_ x: T) -> U where U == T.Element {
  return x.getCElem()
}
func genericReturn<T:D, U>(_ x: T) -> U where U == T.Item {
  return x.getDElem()
}

genericReturn(CDStruct()).#^GENERIC^#

// GENERIC: Begin completions, 4 items
// GENERIC-DAG: Keyword[self]/CurrNominal:          self[#B#]{{; name=.+$}}
// GENERIC-DAG: Decl[InstanceMethod]/CurrNominal:   doBThings()[#Void#]{{; name=.+$}}
// GENERIC-DAG: Keyword[self]/CurrNominal:          self[#A#]{{; name=.+$}}
// GENERIC-DAG: Decl[InstanceMethod]/CurrNominal:   doAThings()[#A#]{{; name=.+$}}
// GENERIC: End completions

genericReturn().#^GENERIC_MISSINGARG^#

// NORESULTS-NOT: Begin completions

struct Point {
    let x: Int
    let y: Int
    var magSquared: Int { return x*y }

    init(_ x: Int, _ y: Int) {
        self.x = x
        self.y = y
    }
}

// POINT_MEMBER: Begin completions, 4 items
// POINT_MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Point#]; name=self
// POINT_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      x[#Int#]; name=x
// POINT_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      y[#Int#]; name=y
// POINT_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      magSquared[#Int#]; name=magSquared
// POINT_MEMBER: End completions

let _ = [Point(1, 4), Point(20, 2), Point(9, -4)]
  .filter { $0.magSquared > 4 }
  .min {
    $0.#^CLOSURE_MISSINGARG^#
  }

protocol SomeProto {}
func testing<T: Collection, U: SomeProto>(_ arg1: T, arg2: (T.Element) -> U) {}
_ = testing([Point(4, 89)]) { arg in
  arg.#^CLOSURE_NORETURN^#
}

struct Thing {
    init(_ block: (Point) -> Void) {}
}
@resultBuilder
struct ThingBuilder {
    static func buildBlock(_ x: Thing...) -> [Thing] { x }
}
func CreateThings(@ThingBuilder makeThings: () -> [Thing]) {}

// In single statement closure
CreateThings {
    Thing { point in
      point.#^CLOSURE_FUNCBUILDER^#
    }
    Thing { _ in }
}

// In multi-statement closure
CreateThings {
    Thing { point in
      print("hello")
      point.#^MULTICLOSURE_FUNCBUILDER^#
    }
    Thing { _ in }
}

// In multi-statement closure with unpropagated errors
CreateThings {
    Thing { point in
      print("hello")
      point. // ErrorExpr
      point.#^MULTICLOSURE_FUNCBUILDER_ERROR^#
    }
    Thing { point in 
      print("hello")
      point. // ErrorExpr
    }
}

// FIXME: No results in multi-statement closure with erroreous sibling result builder element
CreateThings {
    Thing { point in
      print("hello")
      point.#^MULTICLOSURE_FUNCBUILDER_FIXME^#
    }
    Thing. // ErrorExpr
}

