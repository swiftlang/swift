// RUN: %batch-code-completion

protocol FontStyle {}
struct FontStyleOne: FontStyle {}
struct FontStyleTwo: FontStyle {}

extension FontStyle where Self == FontStyleOne {
    static var variableDeclaredInConstraintExtension: FontStyleOne { FontStyleOne() }
}

func foo<T: FontStyle>(x: T) {}
func test() {
    foo(x: .#^COMPLETE_STATIC_MEMBER?check=EXTENSION_APPLIED^#)
}

// EXTENSION_APPLIED: Begin completions, 1 item
// EXTENSION_APPLIED-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: variableDeclaredInConstraintExtension[#FontStyleOne#];

func test<T: FontStyle>(x: T) {
  x.#^COMPLETE_MEMBER_IN_GENERIC_CONTEXT?check=EXTENSION_NOT_APPLIED^#
}

// EXTENSION_NOT_APPLIED: Begin completions, 1 item
// EXTENSION_NOT_APPLIED-DAG: Keyword[self]/CurrNominal:          self[#T#];
// EXTENSION_NOT_APPLIED-NOT: variableDeclaredInConstraintExtension

struct WrapperStruct<T: FontStyle> {
  let y: T

  func test(x: T) {
    x.#^COMPLETE_MEMBER_IN_NESTED_GENERIC_CONTEXT?check=EXTENSION_NOT_APPLIED^#
    y.#^COMPLETE_MEMBER_FROM_OUTER_GENERIC_CONTEXT_IN_INNER?check=EXTENSION_NOT_APPLIED^#
    test(x: .#^COMPLETE_GENERIC_FUNC_WITH_TYPE_SPECIALIZED^#)
    // COMPLETE_GENERIC_FUNC_WITH_TYPE_SPECIALIZED-NOT: variableDeclaredInConstraintExtension
  }
}

func bar<T: FontStyle>(x: T) -> T { return x }
func test2<T: FontStyle>(x: T) {
  bar(x).#^COMPLETE_ON_GENERIC_FUNC_WITH_GENERIC_ARG?check=EXTENSION_NOT_APPLIED^#
  bar(FontStyleTwo()).#^COMPLETE_ON_GENERIC_FUNC^#
  // COMPLETE_ON_GENERIC_FUNC: Begin completions, 1 item
  // COMPLETE_ON_GENERIC_FUNC-DAG: Keyword[self]/CurrNominal:          self[#FontStyleTwo#];
}

// https://github.com/apple/swift/issues/55419

struct Struct {}
struct Indicator<T> {}
extension Indicator where T == Struct {
  static var activity: Indicator<Struct> { fatalError() }
}

func receiver<T>(_ indicator: Indicator<T>) {}

func test() {
  receiver(.#^COMPLETE_GENERIC_TYPE^#)
}
// COMPLETE_GENERIC_TYPE: Begin completions, 2 items
// COMPLETE_GENERIC_TYPE: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#Indicator<T>#];
// COMPLETE_GENERIC_TYPE: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: activity[#Indicator<Struct>#];

func testRecursive<T>(_ indicator: Indicator<T>) {
  testRecursive(.#^COMPLETE_RECURSIVE_GENERIC^#)
// FIXME: We should be suggesting `.activity` here because the call to `testRecursive` happens with new generic parameters
// COMPLETE_RECURSIVE_GENERIC: Begin completions, 1 item
// COMPLETE_RECURSIVE_GENERIC-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#Indicator<T>#];
}
