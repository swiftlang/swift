// RUN: %batch-code-completion

class C1{}
protocol P1{}
struct S1{}

let ValueInt1 = 1
let ValueString2 = ""
func TopLevelFunc() {}

func f1<S : #^INHERIT1?check=INHERIT^#>(p : S) {}
func f2<S : #^INHERIT2?check=INHERIT^#

class C2 {
  func f1<S : #^INHERIT3?check=INHERIT^#>(p : S) {}
  func f2<S : #^INHERIT4?check=INHERIT^#
}

class C3 {
  func f1<S1: P1, S2 : #^INHERIT5?check=INHERIT^#>(p : S1) {}
  func f2<S1: P1, S2 : #^INHERIT6?check=INHERIT^#
}

// INHERIT-DAG: Decl[Class]/CurrModule:             C1[#C1#]{{; name=.+$}}

// FIXME: Exclude struct type. rdar://24110708
// INHERIT-DAG: Decl[Struct]/CurrModule:            S1[#S1#]{{; name=.+$}}
// INHERIT-DAG: Decl[Protocol]/CurrModule:          P1[#P1#]{{; name=.+$}}
// INHERIT-NOT: ValueInt1
// INHERIT-NOT: ValueString2
// INHERIT-NOT: TopLevelFunc

// https://github.com/apple/swift/issues/56788

class C4<T, U> {}

_ = C4<#^GENERIC_TYPE_PARAM^# >()
_ = C4<SomeType, #^SECOND_GENERIC_TYPE_PARAM?check=GENERIC_TYPE_PARAM^# >()
// GENERIC_TYPE_PARAM-DAG: Decl[Class]/CurrModule:             C1[#C1#];

// https://github.com/apple/swift/issues/56979

struct S2 {
  struct Nested<Elements> {
    init() {}
  }
}

var s2_globalVar = S2.Nested< #^GENERIC_PARAM_ON_NESTED_TYPE_GLOBAL_VAR?check=GENERIC_PARAM_ON_NESTED_TYPE^#>()

func someFunction() {
  var s2_localVar = S2.Nested< #^GENERIC_PARAM_ON_NESTED_TYPE_LOCAL_VAR?check=GENERIC_PARAM_ON_NESTED_TYPE^#>()
}

// GENERIC_PARAM_ON_NESTED_TYPE-DAG: Decl[Struct]/CurrModule:            S2[#S2#];

func testGenericWithImplicitSome<T>(_ x: some P1) -> #^GENERIC_WITH_IMPLICIT_SOME^# {}

// GENERIC_WITH_IMPLICIT_SOME-DAG: Decl[GenericTypeParam]/Local:       T[#T#]; name=T
