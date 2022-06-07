// RUN: %target-typecheck-verify-swift

@available(*, deprecated)
func deprecatedFunc() {}

@available(*, deprecated)
func deprecatedFuncWithReturnValue() -> Int {
    return 3
}

@available(*, deprecated)
func deprecatedFuncWithReturnValue2() -> String {
    return ""
}

@available(*, deprecated)
var deprecatedVariable = ""

@available(*, deprecated)
struct deprecatedType {}


@ignoreDeprecationWarnings 
// FIXME: deprecation warnings are still diagnosed for top-level variable declaration
// this has to do with Swift diagnosing deprecation warnings for deprecated top-level variables which are assigned to a deprecated property
var topLevelDecl = deprecatedVariable // expected-warning {{'deprecatedVariable' is deprecated}}

@ignoreDeprecationWarnings
func x() {
    deprecatedFunc()
}

let instanceThatSouldProduceWarning = deprecatedType() // expected-warning {{'deprecatedType' is deprecated}}

var s = deprecatedVariable // expected-warning {{'deprecatedVariable' is deprecated}}

func funcThatShouldProduceWarning() {
    deprecatedFunc() // expected-warning {{'deprecatedFunc()' is deprecated}}
}

struct A {
    @ignoreDeprecationWarnings let x = deprecatedFuncWithReturnValue()
    let shouldWarn = deprecatedFuncWithReturnValue() // expected-warning {{'deprecatedFuncWithReturnValue()' is deprecated}}
}

@ignoreDeprecationWarnings
extension A {
    func aFunc() -> Int {
        deprecatedFuncWithReturnValue()
    }
}

@ignoreDeprecationWarnings
class anotherType {
    func a() {
        _ = deprecatedFuncWithReturnValue()
    }

    let shouldntWarn1 = deprecatedType()
    let shouldntWarn2 = deprecatedFuncWithReturnValue2()
}

@discardableResult
@ignoreDeprecationWarnings
func multipleAttrsFunc() -> deprecatedType {
    return deprecatedType()
}

@ignoreDeprecationWarnings
@available(iOS 15.0, *)
var multipleAttrsVar = 3

struct aNormalType {
    var someNumber: Int   
}

protocol aProtocol {
    var multipliedByTwo: Int { get }
}

@available(*, deprecated)
extension aNormalType: aProtocol {
    var multipliedByTwo: Int {
        self.someNumber * 2
    }
}

@ignoreDeprecationWarnings
func b() -> Int {
    return aNormalType(someNumber: 420).multipliedByTwo
}

func useConformance<T: aProtocol>(_ x: T.Type) {}
useConformance(aNormalType.self) // expected-warning {{conformance of 'aNormalType' to 'aProtocol' is deprecated}}

