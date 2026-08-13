// RUN: not %target-swift-frontend -typecheck -swift-version 5 \
// RUN:   -diagnostic-style llvm %s 2>&1 \
// RUN:   | %FileCheck --strict-whitespace --enable-windows-compatibility %s

struct Struct { init(_ x: Int) {} }
func unusedInitResult() { Struct(1) }

func fn(_ x: Int) -> Int { x }
func unusedCallResult() { fn(2) }

func withClosure<T>(_ v: T, _ body: (T) -> T) -> T { body(v) }
func unusedTrailingClosureResult(v: Int) { withClosure(v) { $0 } }

func unusedOperatorResult(a: Int, b: Int) { a + b }

struct Nullary { init() {} }
func extraArgumentToNullaryCall() { _ = Nullary(extra: 1) }

func voidCall() { _ = Void(1) }

func named(a: Int) {}
func extraNamedArgument() { named(a: 1, b: 2) }

func positional(_ a: Int) {}
func extraPositionalArgument() { positional(1, 2) }

func tupleParam(x: (Int, Int)) {}
func tupleSplat() { tupleParam(x: 0, 1) }

func labeled(a: Int, b: Int) {}
func outOfOrderArguments() { labeled(b: 1, a: 2) }

@resultBuilder struct Builder { static func buildBlock(_ x: Int...) -> Int { 0 } }
@Builder(1) func resultBuilderArguments() -> Int { 1 }

@unsafe @safe func safeAndUnsafe() {}

@available(*, unavailable)
func unavailableFn() {}

func useUnavailable() { unavailableFn() }

@available(swift, obsoleted: 3.0)
func obsoletedFn() {}

func useObsoleted() { obsoletedFn() }

@available(swift, introduced: 99)
func unintroducedFn() {}

func useUnintroduced() { unintroducedFn() }

protocol P {}
func requiresP<T: P>(_ t: T) {}

struct Unavail {}
@available(*, unavailable)
extension Unavail: P {}

func useUnavailableConformance(v: Unavail) { requiresP(v) }

struct Obs {}
@available(swift, obsoleted: 3.0)
extension Obs: P {}

func useObsoletedConformance(v: Obs) { requiresP(v) }

// CHECK:      error: result builder attributes cannot have arguments
// CHECK-NEXT: {{^}}@Builder(1) func resultBuilderArguments() -> Int { 1 }
// CHECK-NEXT: {{^}}^~~~~~~~~~~{{$}}

// CHECK:      error: global function 'safeAndUnsafe' cannot be both '@safe' and '@unsafe'
// CHECK-NEXT: {{^}}@unsafe @safe func safeAndUnsafe() {}
// CHECK-NEXT: {{^}}~~~~~~~ ~~~~~      ^{{$}}

// CHECK:      warning: result of 'Struct' initializer is unused
// CHECK-NEXT: {{^}}func unusedInitResult() { Struct(1) }
// CHECK-NEXT: {{^}}                          ^~~~~~~~~{{$}}

// CHECK:      warning: result of call to 'fn' is unused
// CHECK-NEXT: {{^}}func unusedCallResult() { fn(2) }
// CHECK-NEXT: {{^}}                          ^~~~~{{$}}

// CHECK:      warning: result of call to 'withClosure' is unused
// CHECK-NEXT: {{^}}func unusedTrailingClosureResult(v: Int) { withClosure(v) { $0 } }
// CHECK-NEXT: {{^}}                                           ^~~~~~~~~~~~~~~~~~~~~{{$}}

// CHECK:      warning: result of operator '+' is unused
// CHECK-NEXT: {{^}}func unusedOperatorResult(a: Int, b: Int) { a + b }
// CHECK-NEXT: {{^}}                                            ~ ^ ~{{$}}

// CHECK:      error: argument passed to call that takes no arguments
// CHECK-NEXT: {{^}}func extraArgumentToNullaryCall() { _ = Nullary(extra: 1) }
// CHECK-NEXT: {{^}}                                        ~~~~~~~~~~~~~~~^~{{$}}

// CHECK:      error: argument passed to call that takes no arguments
// CHECK-NEXT: {{^}}func voidCall() { _ = Void(1) }
// CHECK-NEXT: {{^}}                      ^~~~~~~{{$}}

// CHECK:      error: extra argument 'b' in call
// CHECK-NEXT: {{^}}func extraNamedArgument() { named(a: 1, b: 2) }
// CHECK-NEXT: {{^}}                            ~~~~~~~~~~~~~~~^~{{$}}

// CHECK:      error: extra argument in call
// CHECK-NEXT: {{^}}func extraPositionalArgument() { positional(1, 2) }
// CHECK-NEXT: {{^}}                                 ~~~~~~~~~~~~~~^~{{$}}

// CHECK:      error: global function 'tupleParam' expects a single parameter of type '(Int, Int)'
// CHECK-NEXT: {{^}}func tupleSplat() { tupleParam(x: 0, 1) }
// CHECK-NEXT: {{^}}                    ~~~~~~~~~~^~~~~~~~~{{$}}

// CHECK:      error: argument 'a' must precede argument 'b'
// CHECK-NEXT: {{^}}func outOfOrderArguments() { labeled(b: 1, a: 2) }
// CHECK-NEXT: {{^}}                                     ~   ~~^~~~{{$}}

// CHECK:      error: 'unavailableFn()' is unavailable
// CHECK-NEXT: {{^}}func useUnavailable() { unavailableFn() }
// CHECK-NEXT: {{^}}                        ^~~~~~~~~~~~~{{$}}

// CHECK:      note: 'unavailableFn()' has been explicitly marked unavailable here
// CHECK-NEXT: {{^}}@available(*, unavailable)
// CHECK-NEXT: {{^}}^~~~~~~~~~~~~~~~~~~~~~~~~~{{$}}

// CHECK:      error: 'obsoletedFn()' is unavailable
// CHECK-NEXT: {{^}}func useObsoleted() { obsoletedFn() }
// CHECK-NEXT: {{^}}                      ^~~~~~~~~~~{{$}}

// CHECK:      note: 'obsoletedFn()' was obsoleted in Swift 3.0
// CHECK-NEXT: {{^}}@available(swift, obsoleted: 3.0)
// CHECK-NEXT: {{^}}^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{{$}}

// CHECK:      error: 'unintroducedFn()' is unavailable in Swift
// CHECK-NEXT: {{^}}func useUnintroduced() { unintroducedFn() }
// CHECK-NEXT: {{^}}                         ^~~~~~~~~~~~~~{{$}}

// CHECK:      note: 'unintroducedFn()' was introduced in Swift 99
// CHECK-NEXT: {{^}}@available(swift, introduced: 99)
// CHECK-NEXT: {{^}}^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{{$}}

// CHECK:      error: conformance of 'Unavail' to 'P' is unavailable
// CHECK-NEXT: {{^}}func useUnavailableConformance(v: Unavail) { requiresP(v) }
// CHECK-NEXT: {{^}}                                             ^{{$}}

// CHECK:      note: conformance of 'Unavail' to 'P' has been explicitly marked unavailable here
// CHECK-NEXT: {{^}}@available(*, unavailable)
// CHECK-NEXT: {{^}}^~~~~~~~~~~~~~~~~~~~~~~~~~{{$}}

// CHECK:      error: conformance of 'Obs' to 'P' is unavailable
// CHECK-NEXT: {{^}}func useObsoletedConformance(v: Obs) { requiresP(v) }
// CHECK-NEXT: {{^}}                                       ^{{$}}

// CHECK:      note: conformance of 'Obs' to 'P' was obsoleted in Swift 3.0
// CHECK-NEXT: {{^}}@available(swift, obsoleted: 3.0)
// CHECK-NEXT: {{^}}^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{{$}}
