// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE1 | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER1 | %FileCheck %s -check-prefix=MEMBER1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER2 | %FileCheck %s -check-prefix=MEMBER2

/**
  - keyword: C1, Class
  - recommended: C2
*/
class C1 {
  /**
    - keyword: v1, Int
    - recommended: v2
  */
  var v1 : Int = 0

  /**
    - keyword: v2, Int
    - recommendedover: v1
  */
  var v2 : Int = 0

  /**
    - keyword: f1, func
    - recommended: f2
  */
  func f1() {}

  /**
    - keyword: f2, func
    - recommendedover: f1
  */
  func f2() {}
}

/**
  - keyword: C2, Class
  - recommendedover: C1
*/
class C2 {}

/**
  - keyword: S1, Struct
  - recommendedover: S2
*/
struct S1 {}

/**
  - keyword: S2, Struct
  - recommended: S1
*/
struct S2 {}

/**
  - keyword: E1, Enum
  - recommended: E2
*/
enum E1{}

/**
  - keyword: E2, Enum
  - recommendedover: E1
*/
enum E2{}

/**
  - keyword: S3, Struct
*/
struct S3 {
  /**
    - nonmutatingvariant: fooing
  */
  mutating func foo() {}

  /**
    - mutatingvariant: foo
  */
  func fooing() -> S3 {}
}

func foo1() {
  #^TYPE1^#
// TYPE1: Begin completions
// TYPE1-DAG: Decl[Class]/CurrModule/keyword[C1, Class]/recommended[C2]: C1[#C1#]
// TYPE1-DAG: Decl[Struct]/CurrModule/keyword[S1, Struct]/recommendedover[S2]: S1[#S1#]
// TYPE1-DAG: Decl[Enum]/CurrModule/keyword[E2, Enum]/recommendedover[E1]: E2[#E2#]
// TYPE1-DAG: Decl[Struct]/CurrModule/keyword[S2, Struct]/recommended[S1]: S2[#S2#]
// TYPE1-DAG: Decl[Class]/CurrModule/keyword[C2, Class]/recommendedover[C1]: C2[#C2#]
// TYPE1-DAG: Decl[Enum]/CurrModule/keyword[E1, Enum]/recommended[E2]: E1[#E1#]
// TYPE1-DAG: Decl[Struct]/CurrModule/keyword[S3, Struct]: S3[#S3#]
}

func foo2() {
  let c = C1()
  c.#^MEMBER1^#
// MEMBER1: Begin completions
// MEMBER1-NEXT: Keyword[self]/CurrNominal: self[#C1#]; name=self
// MEMBER1-NEXT: Decl[InstanceVar]/CurrNominal/keyword[v1, Int]/recommended[v2]: v1[#Int#]
// MEMBER1-NEXT: Decl[InstanceVar]/CurrNominal/keyword[v2, Int]/recommendedover[v1]: v2[#Int#]
// MEMBER1-NEXT: Decl[InstanceMethod]/CurrNominal/keyword[f1, func]/recommended[f2]: f1()[#Void#]
// MEMBER1-NEXT: Decl[InstanceMethod]/CurrNominal/keyword[f2, func]/recommendedover[f1]: f2()[#Void#]
}

func foo3() {
  let s = S3()
  s.#^MEMBER2^#
// MEMBER2: Begin completions
// MEMBER2-NEXT: Keyword[self]/CurrNominal: self[#S3#]; name=self
// MEMBER2-NEXT: Decl[InstanceMethod]/CurrNominal/nonmutatingvariant[fooing]: foo()[#Void#]
// MEMBER2-NEXT: Decl[InstanceMethod]/CurrNominal/mutatingvariant[foo]: fooing()[#S3#]
}
