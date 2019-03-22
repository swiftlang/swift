// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprDot | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprUnresolved | %FileCheck %s -check-prefix=S_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprNewlineDot1 | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprNewlineDot2 | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprCommaDot | %FileCheck %s -check-prefix=S_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprNewlineExpr | %FileCheck %s -check-prefix=LOCAL_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestCallExprSpaceExpr | %FileCheck %s -check-prefix=TestCallExprSpaceExpr

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprDot | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprUnresolved | %FileCheck %s -check-prefix=S_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprNewlineDot1 | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprNewlineDot2 | %FileCheck %s -check-prefix=T_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprCommaDot | %FileCheck %s -check-prefix=S_MEMBERS_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprNewlineExpr | %FileCheck %s -check-prefix=LOCAL_S_CONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSubscriptExprSpaceExpr | %FileCheck %s -check-prefix=TestCallExprSpaceExpr

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testArrayDotExpr | %FileCheck %s -check-prefix=testArrayDotExpr
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testArrayExprNewlineDot1 | %FileCheck %s -check-prefix=testArrayDotExpr
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testArrayExprNewlineDot2 | %FileCheck %s -check-prefix=testArrayExprNewlineDot2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testArrayExprNewlineExpr | %FileCheck %s -check-prefix=LOCAL_S_CONTEXT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testDictDotExpr1 | %FileCheck %s -check-prefix=testDictDotExpr1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testDictDotExpr2 | %FileCheck %s -check-prefix=testDictDotExpr2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testDictExprNewlineDot1 | %FileCheck %s -check-prefix=testDictDotExpr1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testDictExprNewlineDot2 | %FileCheck %s -check-prefix=testDictDotExpr2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=testDictExprNewlineExpr | %FileCheck %s -check-prefix=LOCAL_S_CONTEXT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestTupleExprNewlineDot1 | %FileCheck %s -check-prefix=TestTupleExprNewlineDot1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestTupleExprNewlineDot2 | %FileCheck %s -check-prefix=TestTupleExprNewlineDot2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestTupleExprNewlineExpr | %FileCheck %s -check-prefix=LOCAL_S_CONTEXT

struct S: Hashable {}

// S_MEMBERS_S_CONTEXT: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init()[#S#];

struct T: Hashable {
  func void() {}
  func t() -> T { return self }
  func s() -> S { return S() }
}

// T_MEMBERS_S_CONTEXT: Begin completions
// T_MEMBERS_S_CONTEXT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: void()[#Void#];
// T_MEMBERS_S_CONTEXT-DAG: Decl[InstanceMethod]/CurrNominal: t()[#T#];
// T_MEMBERS_S_CONTEXT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]:   s()[#S#];
// T_MEMBERS_S_CONTEXT: End completions

struct TestCallExprDot {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      s
      t.#^TestCallExprDot^#
    )
  }
}

struct TestCallExprUnresolved {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      .#^TestCallExprUnresolved^#
    )
  }
}

struct TestCallExprNewlineDot1 {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      s
      t
      .#^TestCallExprNewlineDot1^#
    )
  }
}

struct TestCallExprNewlineDot2 {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      t
      .#^TestCallExprNewlineDot2^#
    )
  }
}

struct TestCallExprCommaDot {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      t,
      .#^TestCallExprCommaDot^#
    )
  }
}

struct TestCallExprNewlineExpr {
  func take(_: T, _: S) {}
  func test(s: S, t: T) {
    take(
      t
      #^TestCallExprNewlineExpr^#
    )
  }
// LOCAL_S_CONTEXT: Begin completions
// LOCAL_S_CONTEXT-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: s[#S#];
// LOCAL_S_CONTEXT-DAG: Decl[LocalVar]/Local:               t[#T#];
// LOCAL_S_CONTEXT: End completions
}

struct TestCallExprSpaceExpr {
  func take(_: S, _: S) {}
  func test(s: S, t: T) {
    take(
      t #^TestCallExprSpaceExpr^#
    )
  }

// TestCallExprSpaceExpr: Begin completions
// FIXME: should have type context S
// TestCallExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .void()[#Void#];
// TestCallExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .t()[#T#];
// TestCallExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .s()[#S#];
// TestCallExprSpaceExpr: End completions
}

// MARK: Subscripts

struct TestSubscriptExprDot {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      s
      t.#^TestSubscriptExprDot^#
    ]
  }
}

struct TestSubscriptExprUnresolved {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      .#^TestSubscriptExprUnresolved^#
    ]
  }
}

struct TestSubscriptExprNewlineDot1 {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      s
      t
      .#^TestSubscriptExprNewlineDot1^#
    ]
  }
}

struct TestSubscriptExprNewlineDot2 {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      t
      .#^TestSubscriptExprNewlineDot2^#
    ]
  }
}

struct TestSubscriptExprCommaDot {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      t,
      .#^TestSubscriptExprCommaDot^#
    ]
  }
}

struct TestSubscriptExprNewlineExpr {
  subscript(_: T, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      t
      #^TestSubscriptExprNewlineExpr^#
    ]
  }
}

struct TestSubscriptExprSpaceExpr {
  subscript(_: S, _: S) -> Int { return 0 }
  func test(s: S, t: T) {
    self[
      t #^TestSubscriptExprSpaceExpr^#
    ]
  }

// TestSubscriptExprSpaceExpr: Begin completions
// FIXME: should have type context S
// TestSubscriptExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .void()[#Void#];
// TestSubscriptExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .t()[#T#];
// TestSubscriptExprSpaceExpr-DAG: Decl[InstanceMethod]/CurrNominal:   .s()[#S#];
// TestSubscriptExprSpaceExpr: End completions
}

// MARK: Collection Literals

func testArrayDotExpr(s: S, t: T) {
  _ = [
    s
    t.#^testArrayDotExpr^#
  ]

// testArrayDotExpr: Begin completions
// testArrayDotExpr-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: void()[#Void#];
// testArrayDotExpr-DAG: Decl[InstanceMethod]/CurrNominal: t()[#T#];
// FIXME: should have S context type.
// testArrayDotExpr-DAG: Decl[InstanceMethod]/CurrNominal: s()[#S#];
// testArrayDotExpr: End completions
}

func testArrayExprNewlineDot1(s: S, t: T) {
  _ = [
    s
    t
    .#^testArrayExprNewlineDot1^#
  ]
}

func testArrayExprNewlineDot2(s: S, t: T) {
  _ = [
    t
    .#^testArrayExprNewlineDot2^#
  ]
}
 
// testArrayExprNewlineDot2: Begin completions
// FIXME: this case should retun the following; it's not related to the commas though.
// FIXME: Decl[InstanceMethod]/CurrNominal: t()[#T#];
// FIXME: Decl[InstanceMethod]/CurrNominal: s()[#S#];
// testArrayExprNewlineDot2: End completions

func testArrayExprNewlineExpr(s: S, t: T) {
  _ = [
    s
    #^testArrayExprNewlineExpr^#
  ]
}

func testDictDotExpr1(s: S, t: T) {
  _ = [
    s: t
    t.#^testDictDotExpr1^#
  ]

// testDictDotExpr1: Begin completions
// testDictDotExpr1-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
// testDictDotExpr1-DAG: Decl[InstanceMethod]/CurrNominal: t()[#T#];
// FIXME: should have S context type.
// testDictDotExpr1-DAG: Decl[InstanceMethod]/CurrNominal: s()[#S#];
// testDictDotExpr1: End completions
}

func testDictDotExpr2(s: S, t: T) {
  _ = [
    s: t
    s:
    t.#^testDictDotExpr2^#
  ]

// testDictDotExpr2: Begin completions
// testDictDotExpr2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: void()[#Void#];
// testDictDotExpr2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: t()[#T#];
// testDictDotExpr2-DAG: Decl[InstanceMethod]/CurrNominal:   s()[#S#];
// testDictDotExpr2: End completions
}

func testDictExprNewlineDot1(s: S, t: T) {
  _ = [
    s: t
    t
    .#^testDictExprNewlineDot1^#
  ]
}

func testDictExprNewlineDot2(s: S, t: T) {
  _ = [
    s:
    t
    .#^testDictExprNewlineDot2^#
  ]
}

func testDictExprNewlineExpr(s: S, t: T) {
  _ = [
    s: t
    #^testDictExprNewlineExpr^# : t
  ]


// MARK: Tuples

struct TestTupleExprNewlineDot1 {
  func take(_: (S, S)) {}
  func test(s: S, t: T) {
    take((
      s
      t
      .#^TestTupleExprNewlineDot1^#
    ))
  }
}

// FIXME: should have S type context (doesn't work with comma either).
// TestTupleExprNewlineDot1: Begin completions
// TestTupleExprNewlineDot1-DAG: Decl[InstanceMethod]/CurrNominal:   void()[#Void#];
// TestTupleExprNewlineDot1-DAG: Decl[InstanceMethod]/CurrNominal:   t()[#T#];
// TestTupleExprNewlineDot1-DAG: Decl[InstanceMethod]/CurrNominal:   s()[#S#];
// TestTupleExprNewlineDot1: End completions

struct TestTupleExprNewlineDot2 {
  func take(_: (S, S)) {}
  func test(s: S, t: T) {
    take((
      t
      .#^TestTupleExprNewlineDot2^#
    ))
  }
}

// FIXME: should have S type context (doesn't work with comma either).
// TestTupleExprNewlineDot2: Begin completions
// TestTupleExprNewlineDot2-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: void()[#Void#];
// TestTupleExprNewlineDot2-DAG: Decl[InstanceMethod]/CurrNominal:   t()[#T#];
// TestTupleExprNewlineDot2-DAG: Decl[InstanceMethod]/CurrNominal:   s()[#S#];
// TestTupleExprNewlineDot2: End completions

struct TestTupleExprNewlineExpr {
  func take(_: (T, S)) {}
  func test(s: S, t: T) {
    take((
      t
      #^TestTupleExprNewlineExpr^#
    ))
  }
}
