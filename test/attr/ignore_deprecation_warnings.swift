// RUN: %target-typecheck-verify-swift

@available(*, deprecated)
func deprecatedFunc() {}

@available(*, deprecated)
func deprecatedFuncWithReturnValue() -> Int {
    return 3
}

@available(*, deprecated)
struct deprecatedType {}

@ignoreDeprecationWarnings
func x() {
    deprecatedFunc()
}

@ignoreDeprecationWarnings
let instance = deprecatedType()

let instanceThatSouldProduceWarning = deprecatedType() // expected-warning {{'deprecatedFunc()' is deprecated}}

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