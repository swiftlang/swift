// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -target %target-cpu-apple-macos50 -swift-version 5 > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// REQUIRES: OS=macosx

// CHECK: {{^}}(root version=50

// CHECK-NEXT: {{^}}  (decl version=51 decl=SomeClass
// CHECK-NEXT: {{^}}    (decl version=52 decl=someMethod()
// CHECK-NEXT: {{^}}      (decl version=53 decl=someInnerFunc()
// CHECK-NEXT: {{^}}      (decl version=53 decl=InnerClass
// CHECK-NEXT: {{^}}        (decl version=54 decl=innerClassMethod
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=someStaticProperty
// CHECK-NEXT: {{^}}      (decl version=52 decl=someStaticProperty
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=someStaticPropertyInferredType
// CHECK-NEXT: {{^}}      (decl version=52 decl=someStaticPropertyInferredType
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=multiPatternStaticPropertyA
// CHECK-NEXT: {{^}}      (decl version=52 decl=multiPatternStaticPropertyA
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=someComputedProperty
// CHECK-NEXT: {{^}}      (decl version=52 decl=someComputedProperty
// CHECK-NEXT: {{^}}    (decl version=52 decl=someOtherMethod()
@available(OSX 51, *)
class SomeClass {
  @available(OSX 52, *)
  func someMethod() {

    @available(OSX 53, *)
    func someInnerFunc() { }

    @available(OSX 53, *)
    class InnerClass {
      @available(OSX 54, *)
      func innerClassMethod() { }
    }
  }
  
  func someUnrefinedMethod() { }

  @available(OSX 52, *)
  static var someStaticProperty: Int = 7

  @available(OSX 52, *)
  static var someStaticPropertyInferredType = 7

  @available(OSX 52, *)
  static var multiPatternStaticPropertyA = 7,
             multiPatternStaticPropertyB = 8

  @available(OSX 52, *)
  var someComputedProperty: Int {
    get { }
    set(v) { }
  }

  @available(OSX 52, *)
  func someOtherMethod() { }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=someFunction()
@available(OSX 51, *)
func someFunction() { }

// CHECK-NEXT: {{^}}  (decl version=51 decl=SomeProtocol
// CHECK-NEXT: {{^}}    (decl version=52 decl=protoMethod()
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=protoProperty
// CHECK-NEXT: {{^}}      (decl version=52 decl=protoProperty
@available(OSX 51, *)
protocol SomeProtocol {
  @available(OSX 52, *)
  func protoMethod() -> Int

  @available(OSX 52, *)
  var protoProperty: Int { get }
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=extension.SomeClass
// CHECK-NEXT: {{^}}    (decl version=51 decl=extension.SomeClass
// CHECK-NEXT: {{^}}      (decl version=52 decl=someExtensionFunction()
@available(OSX 51, *)
extension SomeClass {
  @available(OSX 52, *)
  func someExtensionFunction() { }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=functionWithStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability version=52
// CHECK-NEXT: {{^}}      (condition_following_availability version=53
// CHECK-NEXT: {{^}}    (if_then version=53
// CHECK-NEXT: {{^}}      (condition_following_availability version=54
// CHECK-NEXT: {{^}}      (if_then version=54
// CHECK-NEXT: {{^}}        (condition_following_availability version=55
// CHECK-NEXT: {{^}}        (decl version=55 decl=funcInGuardElse()
// CHECK-NEXT: {{^}}        (guard_fallthrough version=55
// CHECK-NEXT: {{^}}          (condition_following_availability version=56
// CHECK-NEXT: {{^}}          (guard_fallthrough version=56
// CHECK-NEXT: {{^}}      (decl version=57 decl=funcInInnerIfElse()
// CHECK-NEXT: {{^}}    (decl version=53 decl=funcInOuterIfElse()
@available(OSX 51, *)
func functionWithStmtCondition() {
  if #available(OSX 52, *),
     let x = (nil as Int?),
     #available(OSX 53, *) {
    if #available(OSX 54, *) {
      guard #available(OSX 55, *) else {
        @available(OSX 55, *)
        func funcInGuardElse() { }
      }
      guard #available(OSX 56, *) else { }
    } else {
      @available(OSX 57, *)
      func funcInInnerIfElse() { }
    }
  } else {
    @available(OSX 53, *)
    func funcInOuterIfElse() { }
  }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=functionWithUnnecessaryStmtCondition
// CHECK-NEXT: {{^}}    (condition_following_availability version=53
// CHECK-NEXT: {{^}}    (if_then version=53
// CHECK-NEXT: {{^}}    (condition_following_availability version=54
// CHECK-NEXT: {{^}}    (if_then version=54

@available(OSX 51, *)
func functionWithUnnecessaryStmtCondition() {
  // Shouldn't introduce availability scope for then branch when unnecessary
  if #available(OSX 51, *) {
  }

  if #available(OSX 10.9, *) {
  }

  // Nested in conjunctive statement condition
  if #available(OSX 53, *),
     let x = (nil as Int?),
     #available(OSX 52, *) {
  }

  if #available(OSX 54, *),
     #available(OSX 54, *) {
  }

  // Wildcard is same as minimum deployment target
  if #available(iOS 7.0, *) {
  }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=functionWithUnnecessaryStmtConditionsHavingElseBranch
// CHECK-NEXT: {{^}}    (if_else version=none
// CHECK-NEXT: {{^}}      (decl version=none decl=funcInInnerIfElse()
// CHECK-NEXT: {{^}}    (if_else version=none
// CHECK-NEXT: {{^}}    (guard_else version=none
// CHECK-NEXT: {{^}}    (guard_else version=none
// CHECK-NEXT: {{^}}    (if_else version=none

@available(OSX 51, *)
func functionWithUnnecessaryStmtConditionsHavingElseBranch(p: Int?) {
  // Else branch context version is bottom when check is unnecessary
  if #available(OSX 51, *) {
  } else {
    if #available(OSX 52, *) {
    }

    @available(OSX 52, *)
    func funcInInnerIfElse() { }

    if #available(iOS 7.0, *) {
    } else {
    }
  }

  if #available(iOS 7.0, *) {
  } else {
  }

  guard #available(iOS 8.0, *) else { }

  // Else branch will execute if p is nil, so it is not dead.
  if #available(iOS 7.0, *),
     let x = p {
  } else {
  }

  if #available(iOS 7.0, *),
     let x = p,
     #available(iOS 7.0, *) {
  } else {
  }

  // Else branch is dead
  guard #available(iOS 7.0, *),
        #available(iOS 8.0, *) else { }

  if #available(OSX 51, *),
     #available(OSX 51, *) {
  } else {
  }

}

// CHECK-NEXT: {{^}}  (decl version=51 decl=functionWithWhile()
// CHECK-NEXT: {{^}}    (condition_following_availability version=52
// CHECK-NEXT: {{^}}    (while_body version=52
// CHECK-NEXT: {{^}}      (decl version=54 decl=funcInWhileBody()
@available(OSX 51, *)
func functionWithWhile() {
  while #available(OSX 52, *),
        let x = (nil as Int?) {
    @available(OSX 54, *)
    func funcInWhileBody() { }
  }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=functionWithDefer()
// CHECK-NEXT: {{^}}    (condition_following_availability version=52
// CHECK-NEXT: {{^}}    (if_then version=52
@available(OSX 51, *)
func functionWithDefer() {
  defer {
    if #available(OSX 52, *) {}
  }
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=extension.SomeClass
// CHECK-NEXT: {{^}}    (decl version=51 decl=extension.SomeClass
// CHECK-NEXT: {{^}}      (decl_implicit version=51 decl=someStaticPropertyWithClosureInit
// CHECK-NEXT: {{^}}        (decl version=52 decl=someStaticPropertyWithClosureInit
// CHECK-NEXT: {{^}}          (condition_following_availability version=54
// CHECK-NEXT: {{^}}          (if_then version=54
// CHECK-NEXT: {{^}}      (decl_implicit version=51 decl=someStaticPropertyWithClosureInitInferred
// CHECK-NEXT: {{^}}        (decl version=52 decl=someStaticPropertyWithClosureInitInferred
// CHECK-NEXT: {{^}}          (condition_following_availability version=54
// CHECK-NEXT: {{^}}          (if_then version=54
@available(OSX 51, *)
extension SomeClass {
  @available(OSX 52, *)
  static var someStaticPropertyWithClosureInit: Int = {
    if #available(OSX 54, *) {
      return 54
    }
    return 53
  }()

  @available(OSX 52, *)
  static var someStaticPropertyWithClosureInitInferred = {
    if #available(OSX 54, *) {
      return 54
    }
    return 53
  }()
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=extension.SomeClass
// CHECK-NEXT: {{^}}    (decl version=50 unavailable=macOS decl=extension.SomeClass
// CHECK-NEXT: {{^}}      (decl version=50 unavailable=macOS decl=functionWithStmtConditionsInUnavailableExt()
// CHECK-NEXT: {{^}}        (condition_following_availability version=52 unavailable=macOS
// CHECK-NEXT: {{^}}          (condition_following_availability version=53 unavailable=macOS
// CHECK-NEXT: {{^}}        (if_then version=53 unavailable=macOS
// CHECK-NEXT: {{^}}          (condition_following_availability version=54 unavailable=macOS
// CHECK-NEXT: {{^}}          (if_then version=54 unavailable=macOS
// CHECK-NEXT: {{^}}            (condition_following_availability version=55 unavailable=macOS
// CHECK-NEXT: {{^}}            (decl version=54 unavailable=macOS decl=funcInGuardElse()
// CHECK-NEXT: {{^}}            (guard_fallthrough version=55 unavailable=macOS
// CHECK-NEXT: {{^}}              (condition_following_availability version=56 unavailable=macOS
// CHECK-NEXT: {{^}}              (guard_fallthrough version=56 unavailable=macOS
// CHECK-NEXT: {{^}}          (decl version=53 unavailable=macOS decl=funcInInnerIfElse()
// CHECK-NEXT: {{^}}        (decl version=50 unavailable=macOS decl=funcInOuterIfElse()
@available(OSX, unavailable)
extension SomeClass {
  @available(OSX 51, *)
  func functionWithStmtConditionsInUnavailableExt() {
    if #available(OSX 52, *),
       let x = (nil as Int?),
       #available(OSX 53, *) {
      if #available(OSX 54, *) {
        guard #available(OSX 55, *) else {
          @available(OSX 55, *)
          func funcInGuardElse() { }
        }
        guard #available(OSX 56, *) else { }
      } else {
        @available(OSX 57, *)
        func funcInInnerIfElse() { }
      }
    } else {
      @available(OSX 53, *)
      func funcInOuterIfElse() { }
    }
  }
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=wrappedValue

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=SomeStruct
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=someLazyVar
// CHECK-NEXT: {{^}}      (condition_following_availability version=52
// CHECK-NEXT: {{^}}      (guard_fallthrough version=52
// CHECK-NEXT: {{^}}    (decl_implicit version=51 decl=someWrappedVar
// CHECK-NEXT: {{^}}      (condition_following_availability version=52
// CHECK-NEXT: {{^}}      (guard_fallthrough version=52
// CHECK-NEXT: {{^}}    (decl version=52 decl=someMethodAvailable52()
@available(OSX 51, *)
struct SomeStruct {
  lazy var someLazyVar = {
    guard #available(OSX 52, *) else {
      return someMethod()
    }
    return someMethodAvailable52()
  }()

  @Wrapper var someWrappedVar = {
    guard #available(OSX 52, *) else {
      return 51
    }
    return 52
  }()

  func someMethod() -> Int { return 51 }

  @available(OSX 52, *)
  func someMethodAvailable52() -> Int { return 52 }
}

// CHECK-NEXT: {{^}}  (decl version=51 decl=SomeEnum
// CHECK-NEXT: {{^}}    (decl version=52 decl=a
// CHECK-NEXT: {{^}}    (decl version=53 decl=b

@available(OSX 51, *)
enum SomeEnum {
  @available(OSX 52, *)
  case a

  @available(OSX 53, *)
  case b, c
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=someComputedGlobalVar
// CHECK-NEXT: {{^}}    (decl version=51 decl=_
// CHECK-NEXT: {{^}}    (decl version=52 decl=_

var someComputedGlobalVar: Int {
  @available(OSX 51, *)
  get { 1 }

  @available(OSX 52, *)
  set { }
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=interpolated
// CHECK-NEXT: {{^}}    (decl_implicit version=50 decl=string

func testStringInterpolation() {
  let interpolated = """
    \([""].map {
      let string = $0
      return string
    })
    """
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=result
// CHECK-NEXT: {{^}}    (decl_implicit version=50 decl=unusedA
// CHECK-NEXT: {{^}}    (decl_implicit version=50 decl=unusedB

func testSequenceExprs(b: Bool, x: Int?) {
  let result = b
    ? x.map {
        let unusedA: Int
        return $0
      }
    : x.map {
        let unusedB: Int
        return $0
      }
}

// CHECK-NEXT: {{^}}  (decl version=50 unavailable=macOS decl=unavailableOnMacOS()
// CHECK-NEXT: {{^}}    (decl_implicit version=50 unavailable=macOS decl=x

@available(macOS, unavailable)
func unavailableOnMacOS() {
  let x = 1
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=extension.SomeEnum
// CHECK-NEXT: {{^}}    (decl version=51 decl=extension.SomeEnum
// CHECK-NEXT: {{^}}      (decl_implicit version=51 decl=unavailableOnMacOS
// CHECK-NEXT: {{^}}        (decl version=51 unavailable=macOS decl=unavailableOnMacOS
@available(OSX 51, *)
extension SomeEnum {
  @available(macOS, unavailable)
  var unavailableOnMacOS: Int { 1 }
}

// CHECK-NEXT: {{^}}  (decl_implicit version=50 decl=extension.SomeEnum
// CHECK-NEXT: {{^}}    (decl version=50 unavailable=macOS decl=extension.SomeEnum
// CHECK-NEXT: {{^}}      (decl_implicit version=50 unavailable=macOS decl=availableMacOS_52
// CHECK-NEXT: {{^}}        (decl version=50 unavailable=macOS decl=availableMacOS_52
// CHECK-NEXT: {{^}}      (decl version=50 unavailable=* decl=neverAvailable()

@available(macOS, unavailable)
extension SomeEnum {
  @available(OSX 52, *)
  var availableMacOS_52: Int { 1 }

  @available(macOSApplicationExtension, unavailable)
  func unavailableInAppExtensions() {}

  @available(*, unavailable)
  func neverAvailable() {}
}

// CHECK-NEXT: {{^}}  (decl version=50 unavailable=macOS decl=unavailableOnMacOSAndIntroduced()

@available(macOS, unavailable)
@available(macOS, introduced: 52)
func unavailableOnMacOSAndIntroduced() {
}

// CHECK-NEXT: {{^}}  (decl version=50 unavailable=macOS decl=introducedOnMacOSAndUnavailable()

@available(macOS, introduced: 53)
@available(macOS, unavailable)
func introducedOnMacOSAndUnavailable() {
}


// CHECK-NEXT: {{^}}  (decl version=50 unavailable=macOS decl=unavailableOnMacOSAndIntroducedSameAttr()

@available(macOS, unavailable, introduced: 54)
func unavailableOnMacOSAndIntroducedSameAttr() {
}

// CHECK-NEXT: {{^}}  (decl version=50 unavailable=* decl=NeverAvailable
// CHECK-NEXT: {{^}}    (decl version=50 unavailable=* decl=unavailableOnMacOS()

@available(*, unavailable)
struct NeverAvailable {
  @available(macOS, unavailable)
  func unavailableOnMacOS() {}
}

// CHECK-NEXT: {{^}}  (decl version=50 deprecated decl=deprecatedOnMacOS()
// CHECK-NEXT: {{^}}    (decl_implicit version=50 deprecated decl=x

@available(macOS, deprecated)
func deprecatedOnMacOS() {
  let x = 1
}

// Since availableOniOS() doesn't have any active @available attributes it
// shouldn't create a scope.
// CHECK-NOT: availableOniOS

@available(iOS, introduced: 53)
func availableOniOS() { }

// CHECK-NEXT: {{^}}  (decl version=50 decl=availableInSwift5

@available(swift 5)
func availableInSwift5() { }

// CHECK-NEXT: {{^}}  (decl version=50 unavailable=swift decl=availableInSwift6

@available(swift 6)
func availableInSwift6() { }

// CHECK-NEXT: {{^}}  (decl version=51 decl=FinalDecl

@available(OSX 51, *)
typealias FinalDecl = Int
