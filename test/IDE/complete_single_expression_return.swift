// RUN: %batch-code-completion

// MARK: Single-expression closures

struct TestSingleExprClosureRet {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return { () in
      return self.#^TestSingleExprClosureRet^#
    }()
  }

// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprClosureRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
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

// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestSingleExprClosureRetVoid-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: void()[#Void#];
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

// TestSingleExprClosureRetUnresolved-NOT: notMine
// TestSingleExprClosureRetUnresolved: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: myEnum[#MyEnum#];
// TestSingleExprClosureRetUnresolved-NOT: notMine
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
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// NOTE: this differs from the one using a return keyword.
// TestSingleExprClosure-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
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
// TestSingleExprClosureVoid-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprClosureVoid-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// Note: this differs from explicit return by having no type context.
// TestSingleExprClosureVoid-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
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
// TestSingleExprClosureUnresolved-NOT: notMine
// TestSingleExprClosureUnresolved: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: myEnum[#MyEnum#];
// TestSingleExprClosureUnresolved-NOT: notMine

struct TestSingleExprClosureCall {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func take(_: () -> Int) {}

  func test() {
    take {
      self.#^TestSingleExprClosureCall?check=TestSingleExprClosure^#
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
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestSingleExprClosureBinding {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    let fn = {
      self.#^TestSingleExprClosureBinding^#
    }
    return fn()
  }
// Void is always valid in an implicit single expr closure.
// TestSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestExplicitSingleExprClosureBinding {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    let fn = {
      return self.#^TestExplicitSingleExprClosureBinding^#
    }
  }
// We have an explicit return, and no expected type, so we don't suggest Void.
// TestExplicitSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestExplicitSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestExplicitSingleExprClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
}

struct TestExplicitMultiStmtClosureBinding {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    let fn = {
      ()
      return self.#^TestExplicitMultiStmtClosureBinding^#
    }
  }
// We have an explicit return, and no expected type, so we don't suggest Void.
// TestExplicitMultiStmtClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestExplicitMultiStmtClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestExplicitMultiStmtClosureBinding-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
}

struct TestExplicitSingleExprClosureBindingWithContext {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    let fn: () -> Void = {
      return self.#^TestExplicitSingleExprClosureBindingWithContext^#
    }
  }
// We know Void is valid.
// TestExplicitSingleExprClosureBindingWithContext-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestExplicitSingleExprClosureBindingWithContext-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestExplicitSingleExprClosureBindingWithContext-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: void()[#Void#];
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
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestNonSingleExprClosureGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
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

// MARK: Single-expression functions

struct TestSingleExprFuncRet {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    return self.#^TestSingleExprFuncRet^#
  }

// TestSingleExprFuncRet-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprFuncRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprFuncRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
}

struct TestSingleExprFunc {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    self.#^TestSingleExprFunc^#
  }

// TestSingleExprFunc-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprFunc-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprFunc-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestSingleExprFuncReturnVoid {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() {
    return self.#^TestSingleExprFuncReturnVoid^#
  }

// Void is the only possible type that can be used here.
// TestSingleExprFuncReturnVoid-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprFuncReturnVoid-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestSingleExprFuncReturnVoid-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: void()[#Void#];
}

struct TestSingleExprFuncUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() -> MyEnum {
    .#^TestSingleExprFuncUnresolved^#
  }

// TestSingleExprFuncUnresolved-NOT: notMine
// TestSingleExprFuncUnresolved: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: myEnum[#MyEnum#];
// TestSingleExprFuncUnresolved-NOT: notMine
}

struct TestNonSingleExprFuncUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() -> MyEnum {
    .#^TestNonSingleExprFuncUnresolved^#
    return .myEnum
  }

// TestNonSingleExprFuncUnresolved-NOT: myEnum
// TestNonSingleExprFuncUnresolved-NOT: notMine
}

struct TestSingleExprLocalFuncUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() {
    func local() -> MyEnum {
      .#^TestSingleExprLocalFuncUnresolved?check=TestSingleExprFuncUnresolved^#
    }
  }
}

struct TestSingleExprFuncGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    #^TestSingleExprFuncGlobal^#
  }
// TestSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestNonSingleExprFuncGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  func test() -> Int {
    #^TestNonSingleExprFuncGlobal^#
    return 42
  }
// TestNonSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestNonSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal: int()[#Int#];
// TestNonSingleExprFuncGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

// MARK: Single-expression accessors

struct TestSingleExprAccessorRet {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  var test: Int {
    return self.#^TestSingleExprAccessorRet^#
  }

// TestSingleExprAccessorRet-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprAccessorRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprAccessorRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
}

struct TestSingleExprAccessor {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  var test: Int {
    self.#^TestSingleExprAccessor^#
  }

// TestSingleExprAccessor-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprAccessor-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprAccessor-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestSingleExprAccessorUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  var test: MyEnum {
    .#^TestSingleExprAccessorUnresolved^#
  }

// TestSingleExprAccessorUnresolved-NOT: notMine
// TestSingleExprAccessorUnresolved: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: myEnum[#MyEnum#];
// TestSingleExprAccessorUnresolved-NOT: notMine
}

struct TestNonSingleExprAccessorUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  var test: MyEnum {
    .#^TestNonSingleExprAccessorUnresolved^#
    return .myEnum
  }

// TestNonSingleExprAccessorUnresolved-NOT: myEnum
// TestNonSingleExprAccessorUnresolved-NOT: notMine
}

struct TestSingleExprLocalAccessorUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  func test() {
    var test: MyEnum {
      .#^TestSingleExprLocalAccessorUnresolved?check=TestSingleExprAccessorUnresolved^#
    }
  }
}

struct TestSingleExprAccessorGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  var test: Int {
    #^TestSingleExprAccessorGlobal^#
  }
// TestSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestNonSingleExprAccessorGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  var test: Int {
    #^TestNonSingleExprAccessorGlobal^#
    return 42
  }

// TestNonSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// FIXME: should should not have type context.
// TestNonSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestNonSingleExprAccessorGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestSingleExprAccessorGetUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  var test: MyEnum {
    get {
      .#^TestSingleExprAccessorGetUnresolved?check=TestSingleExprAccessorUnresolved^#
    }
  }
}

struct TestSingleExprAccessorGetGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  var test: Int {
    get {
      #^TestSingleExprAccessorGetGlobal?check=TestSingleExprAccessorGlobal^#
    }
  }
}

struct TestSingleExprAccessorSetUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  var test: MyEnum {
    set {
      .#^TestSingleExprAccessorSetUnresolved^#
    }
  }
// TestSingleExprAccessorSetUnresolved-NOT: myEnum
// TestSingleExprAccessorSetUnresolved-NOT: notMine
}

// MARK: Single-expression subscripts

struct TestSingleExprSubscriptRet {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  subscript(_: Int) -> Int {
    return self.#^TestSingleExprSubscriptRet^#
  }

// TestSingleExprSubscriptRet-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprSubscriptRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprSubscriptRet-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: void()[#Void#];
}

struct TestSingleExprSubscript {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  subscript(_: Int) -> Int {
    self.#^TestSingleExprSubscript^#
  }

// TestSingleExprSubscript-DAG: Decl[InstanceMethod]/CurrNominal:   str()[#String#];
// TestSingleExprSubscript-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprSubscript-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

struct TestSingleExprSubscriptUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  subscript(_: Int) -> MyEnum {
    .#^TestSingleExprSubscriptUnresolved^#
  }

// TestSingleExprSubscriptUnresolved-NOT: notMine
// TestSingleExprSubscriptUnresolved: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: myEnum[#MyEnum#];
// TestSingleExprSubscriptUnresolved-NOT: notMine
}

struct TestNonSingleExprSubscriptUnresolved {
  enum MyEnum { case myEnum }
  enum NotMine { case notMine }
  func mine() -> MyEnum { return .myEnum }
  func notMine() -> NotMine { return .notMine }

  subscript(_: Int) -> MyEnum {
    .#^TestNonSingleExprSubscriptUnresolved^#
    return .myEnum
  }

// TestNonSingleExprSubscriptUnresolved-NOT: myEnum
// TestNonSingleExprSubscriptUnresolved-NOT: notMine
}

struct TestSingleExprSubscriptGlobal {
  func void() -> Void {}
  func str() -> String { return "" }
  func int() -> Int { return 0 }

  subscript(input: Int) -> Int {
    #^TestSingleExprSubscriptGlobal^#
  }

// TestSingleExprSubscriptGlobal-DAG: Decl[InstanceMethod]/CurrNominal: str()[#String#];
// TestSingleExprSubscriptGlobal-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: int()[#Int#];
// TestSingleExprSubscriptGlobal-DAG: Decl[InstanceMethod]/CurrNominal: void()[#Void#];
}

// MARK: Single-expression initializers

enum TestSingeExprInitInvalid {
  case foo
  init() {
    .#^TestSingeExprInitInvalid^#
  }
// TestSingeExprInitInvalid-NOT: foo
}

enum TestSingeExprInitNone {
  case foo
  init?() {
    .#^TestSingeExprInitNone^#
  }
// TestSingeExprInitNone-NOT: foo
// Note: only `nil` is allowed here, not `.none`.
// TestSingeExprInitNone-NOT: none
}

enum TestSingeExprInitNilRet {
  case foo
  init?() {
    return #^TestSingeExprInitNilRet^#
  }
// TestSingeExprInitNilRet: Literal[Nil]/None/TypeRelation[Convertible]: nil[#TestSingeExprInitNil{{(Ret)?}}?#];
}

enum TestSingeExprInitNil {
  case foo
  init?() {
    #^TestSingeExprInitNil^#
  }
// FIXME: For consistency, this should be same as TestSingeExprInitNilRet.
// TestSingeExprInitNil: Literal[Nil]/None: nil;
}

enum TestNonSingeExprInitNil1 {
  case foo
  init?() {
    #^TestNonSingeExprInitNil1?check=TestNonSingeExprInitNil^#
    return nil
  }
// No type relation.
// TestNonSingeExprInitNil: Literal[Nil]/None: nil;
}

enum TestNonSingeExprInitNil2 {
  case foo
  init?() {
    #^TestNonSingeExprInitNil2?check=TestNonSingeExprInitNil^#
    self = .foo
  }
}

enum TestSingeExprDeinitInvalid {
  case foo
  deinit {
    .#^TestSingeExprDeinitInvalid^#
  }
// TestSingeExprDeinitInvalid-NOT: foo
}

// MARK: Top-level code

enum TopLevelEnum {
  case foo
}

// TopLevelEnum: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: foo[#TopLevelEnum#];

var testAccessorUnresolvedTopLevel: TopLevelEnum {
  .#^testAccessorUnresolvedTopLevel?check=TopLevelEnum^#
}

var testAccessorUnresolvedTopLevelGet: TopLevelEnum {
  get {
    .#^testAccessorUnresolvedTopLevelGet?check=TopLevelEnum^#
  }
}

var testClosureUnresolvedTopLevelInit: TopLevelEnum = {
  .#^testClosureUnresolvedTopLevelInit?check=TopLevelEnum^#
}()
