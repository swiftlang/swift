// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE_EXTRAARG | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=SIMPLE_MEMBERS | %FileCheck %s --check-prefix=SIMPLE
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=RELATED | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=RELATED_EXTRAARG | %FileCheck %s --check-prefix=RELATED
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=GENERIC | %FileCheck %s --check-prefix=GENERIC
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_MISSINGARG | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_NORETURN | %FileCheck %s --check-prefix=POINT_MEMBER
// RUN: %swift-ide-test -code-completion  -source-filename %s -code-completion-token=CLOSURE_FUNCBUILDER | %FileCheck %s --check-prefix=POINT_MEMBER

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

// FIXME: this takes 19 seconds to determine there are no results.
genericReturn().#^GENERIC_MISSINGARG^#

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
@_functionBuilder
struct ThingBuilder {
    static func buildBlock(_ x: Thing...) -> [Thing] { x }
}
func CreateThings(@ThingBuilder makeThings: () -> [Thing]) {}

// FIXME: only works in single expression closure
CreateThings {
    Thing { point in
      point.#^CLOSURE_FUNCBUILDER^#
    }
    Thing { _ in }
}

