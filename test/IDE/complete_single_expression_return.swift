// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureRet | %FileCheck %s -check-prefix=TestSingleExprClosureRet
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureRetVoid | %FileCheck %s -check-prefix=TestSingleExprClosureRetVoid
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureRetUnresolved | %FileCheck %s -check-prefix=TestSingleExprClosureRetUnresolved
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosure | %FileCheck %s -check-prefix=TestSingleExprClosure
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureVoid | %FileCheck %s -check-prefix=TestSingleExprClosureRetVoid
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureUnresolved | %FileCheck %s -check-prefix=TestSingleExprClosureRetUnresolved
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureCall | %FileCheck %s -check-prefix=TestSingleExprClosure
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestSingleExprClosureGlobal | %FileCheck %s -check-prefix=TestSingleExprClosureGlobal
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestNonSingleExprClosureGlobal | %FileCheck %s -check-prefix=TestNonSingleExprClosureGlobal
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TestNonSingleExprClosureUnresolved | %FileCheck %s -check-prefix=TestNonSingleExprClosureUnresolved

struct TestSingleExprClosureRet {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return { () in
      return self.#^TestSingleExprClosureRet^#
    }()
  }

// TestSingleExprClosureRet: Begin completions
// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: int()[#Int#];
// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: void()[#Void#];
// TestSingleExprClosureRet: End completions
}

struct TestSingleExprClosureRetVoid {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    return { () in
      return self.#^TestSingleExprClosureRetVoid^#
    }()
  }

// TestSingleExprClosureRetVoid: Begin completions
// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: void()[#Void#];
// TestSingleExprClosureRetVoid: End completions
}

struct TestSingleExprClosureRetUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() -> MyEnum {
    return { () in
      return .#^TestSingleExprClosureRetUnresolved^#
    }()
  }

// TestSingleExprClosureRetUnresolved: Begin completions
// TestSingleExprClosureRetUnresolved-NOT: notMine
// TestSingleExprClosureRetUnresolved: Decl[EnumElement]/ExprSpecific: myEnum[#TestSingleExprClosure{{(Ret)?}}Unresolved.MyEnum#];
// TestSingleExprClosureRetUnresolved-NOT: notMine
// TestSingleExprClosureRetUnresolved: End completions
}

struct TestSingleExprClosure {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return { () in
      self.#^TestSingleExprClosure^#
    }()
  }
// TestSingleExprClosure: Begin completions
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: int()[#Int#];
// NOTE: this differs from the one using a return keyword.
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
// TestSingleExprClosure: End completions
}

struct TestSingleExprClosureVoid {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    return { () in
      self.#^TestSingleExprClosureVoid^#
    }()
  }
}

struct TestSingleExprClosureUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() -> MyEnum {
    return { () in
      .#^TestSingleExprClosureUnresolved^#
    }()
  }
}

struct TestSingleExprClosureCall {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func take(_: () -> Int) {}

  func test() {
    take {
      self.#^TestSingleExprClosureCall^#
    }
  }
}

struct TestSingleExprClosureGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return { () in
      #^TestSingleExprClosureGlobal^#
    }()
  }
// TestSingleExprClosureGlobal: Begin completions
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal:   str()[#String#];
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal/TypeRelation[Identical]: int()[#Int#];
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal: void()[#Void#];
// TestSingleExprClosureGlobal: End completions
}

struct TestNonSingleExprClosureGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return { () in
      #^TestNonSingleExprClosureGlobal^#
      return 42
    }()
  }
// TestNonSingleExprClosureGlobal: Begin completions
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal: str()[#String#];
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal: int()[#Int#];
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/OutNominal: void()[#Void#];
// TestNonSingleExprClosureGlobal: End completions
}

struct TestNonSingleExprClosureUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() -> Int {
    return { () in
      .#^TestNonSingleExprClosureUnresolved^#
      return 42
    }()
  }
// TestNonSingleExprClosureUnresolved-NOT: myEnum
// TestNonSingleExprClosureUnresolved-NOT: notMine
}
