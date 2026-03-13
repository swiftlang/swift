// RUN: %target-typecheck-verify-swift

@resultBuilder // expected-error {{'@resultBuilder' attribute cannot be applied to this declaration}}
var globalBuilder: Int

@resultBuilder // expected-error {{'@resultBuilder' attribute cannot be applied to this declaration}}
func globalBuilderFunction() -> Int { return 0 }

@resultBuilder
struct Maker {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

@resultBuilder
class Inventor {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

@Maker // expected-error {{result builder attribute 'Maker' can only be applied to a parameter, function, subscript, or computed property}}
typealias typename = Inventor

@Maker // expected-error {{result builder attribute 'Maker' can only be applied to a variable if it defines a getter}}
var global: Int

@Maker
var globalWithEmptyImplicitGetter: Int {}
// expected-error@-1{{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
// Note: no missing return error is expected in this case. Similar test added to `SILOptimizer/missing_returns` to verify SIL diagnostic behavior.

@Maker
var globalWithEmptyExplicitGetter: Int { get {} }  // expected-error{{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}

@Maker
var globalWithSingleGetter: Int { 0 } // expected-error {{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}

@Maker
var globalWithMultiGetter: Int { 0; 0 } // expected-error {{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}

@Maker
func globalFunction() {} // expected-error {{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}

@Maker
func globalFunctionWithFunctionParam(fn: () -> ()) {}  // expected-error {{result builder 'Maker' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}

func makerParam(@Maker
                fn: () -> ()) {}

// FIXME: these diagnostics are reversed?
func makerParamRedundant(@Maker // expected-error {{only one result builder attribute can be attached to a parameter}}
                         @Maker // expected-note {{previous result builder specified here}}
                         fn: () -> ()) {}

func makerParamConflict(@Maker // expected-error {{only one result builder attribute can be attached to a parameter}}
                        @Inventor // expected-note {{previous result builder specified here}}
                        fn: () -> ()) {}

func makerParamMissing1(@Missing // expected-error {{unknown attribute 'Missing'}}
                        @Maker
                        fn: () -> ()) {}

func makerParamMissing2(@Maker
                        @Missing // expected-error {{unknown attribute 'Missing'}}
                        fn: () -> ()) {}

func makerParamExtra(@Maker(5) // expected-error {{result builder attributes cannot have arguments}}
                     fn: () -> ()) {}

func makerParamAutoclosure(@Maker // expected-error {{result builder attribute 'Maker' cannot be applied to an autoclosure parameter}}
                           fn: @autoclosure () -> ()) {}

@resultBuilder
struct GenericMaker<T> {} // expected-note {{generic struct 'GenericMaker' declared here}} expected-error {{result builder must provide at least one static 'buildBlock' method}}

struct GenericContainer<T> {  // expected-note {{generic struct 'GenericContainer' declared here}}
  @resultBuilder
  struct Maker {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}
}

func makeParamUnbound(@GenericMaker // expected-error {{reference to generic type 'GenericMaker' requires arguments}}
                      fn: () -> ()) {}

func makeParamBound(@GenericMaker<Int>
                    fn: () -> ()) {}

func makeParamNestedUnbound(@GenericContainer.Maker // expected-error {{reference to generic type 'GenericContainer' requires arguments}}
                            fn: () -> ()) {}

func makeParamNestedBound(@GenericContainer<Int>.Maker
                          fn: () -> ()) {}


protocol P { }

@resultBuilder
struct ConstrainedGenericMaker<T: P> {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}


struct WithinGeneric<U> {
  func makeParamBoundInContext(@GenericMaker<U> fn: () -> ()) {}

  // expected-error@+1{{type 'U' does not conform to protocol 'P'}}
  func makeParamBoundInContextBad(@ConstrainedGenericMaker<U>
    fn: () -> ()) {}
}

@resultBuilder
struct ValidBuilder1 {
  static func buildBlock(_ exprs: Any...) -> Int { return exprs.count }
}

protocol BuilderFuncHelper {}

extension BuilderFuncHelper {
  static func buildBlock(_ exprs: Any...) -> Int { return exprs.count }
}

@resultBuilder
struct ValidBuilder2: BuilderFuncHelper {}

class BuilderFuncBase {
  static func buildBlock(_ exprs: Any...) -> Int { return exprs.count }
}

@resultBuilder
class ValidBuilder3: BuilderFuncBase {}

@resultBuilder
struct ValidBuilder4 {}
extension ValidBuilder4 {
    static func buildBlock(_ exprs: Any...) -> Int { return exprs.count }
}

@resultBuilder
struct ValidBuilder5 {
    static func buildBlock() -> Int { 0 }
}

@resultBuilder
struct InvalidBuilder1 {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

@resultBuilder
struct InvalidBuilder2 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
  func buildBlock(_ exprs: Any...) -> Int { return exprs.count } // expected-note {{did you mean to make instance method 'buildBlock' static?}} {{3-3=static }}
}

@resultBuilder
struct InvalidBuilder3 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
  var buildBlock: (Any...) -> Int = { return $0.count } // expected-note {{potential match 'buildBlock' is not a static method}}
}

@resultBuilder
struct InvalidBuilder4 {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}
extension InvalidBuilder4 {
  func buildBlock(_ exprs: Any...) -> Int { return exprs.count } // expected-note {{did you mean to make instance method 'buildBlock' static?}} {{3-3=static }}
}

protocol InvalidBuilderHelper {}
extension InvalidBuilderHelper {
  func buildBlock(_ exprs: Any...) -> Int { return exprs.count } // expected-note {{potential match 'buildBlock' is not a static method}}
}

@resultBuilder
struct InvalidBuilder5: InvalidBuilderHelper {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

@resultBuilder
struct InvalidBuilder6 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
    static var buildBlock: Int = 0 // expected-note {{potential match 'buildBlock' is not a static method}}
}

struct Callable {
    func callAsFunction(_ exprs: Any...) -> Int { return exprs.count }
}

@resultBuilder
struct InvalidBuilder7 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
    static var buildBlock = Callable() // expected-note {{potential match 'buildBlock' is not a static method}}
}

class BuilderVarBase {
  static var buildBlock: (Any...) -> Int = { return $0.count } // expected-note {{potential match 'buildBlock' is not a static method}}
}

@resultBuilder
class InvalidBuilder8: BuilderVarBase {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

protocol BuilderVarHelper {}

extension BuilderVarHelper {
  static var buildBlock: (Any...) -> Int { { return $0.count } } // expected-note {{potential match 'buildBlock' is not a static method}}
}

@resultBuilder
struct InvalidBuilder9: BuilderVarHelper {} // expected-error {{result builder must provide at least one static 'buildBlock' method}}

@resultBuilder
struct InvalidBuilder10 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
  static var buildBlock: (Any...) -> Int = { return $0.count } // expected-note {{potential match 'buildBlock' is not a static method}}
}

@resultBuilder
enum InvalidBuilder11 { // expected-error {{result builder must provide at least one static 'buildBlock' method}}
    case buildBlock(Any) // expected-note {{enum case 'buildBlock' cannot be used to satisfy the result builder requirement}}
}

@resultBuilder
struct ValidPairwiseBuilder1 {
  static func buildPartialBlock(first: Int) -> Int { fatalError() }
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { fatalError() }
}

@resultBuilder
struct UnavailablePairwiseBuilder {
  @available(*, unavailable)
  static func buildPartialBlock(first: Int) -> Int { fatalError() }
  @available(*, unavailable)
  static func buildPartialBlock(accumulated: Int, next: Int) -> Int { fatalError() }
  // available
  // static func buildBlock(_: Int...) -> Int { fatalError() }
}

@resultBuilder
struct InvalidPairwiseBuilder1 {} // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}

@resultBuilder
struct InvalidPairwiseBuilder2 { // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}
  static func buildPartialBlock(first: Any) -> Any { fatalError() }
}
@resultBuilder
struct InvalidPairwiseBuilder3 { // expected-error {{result builder must provide at least one static 'buildBlock' method, or both 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)'}}
  static func buildPartialBlock(accumulated: Any, next: Any) -> Any { fatalError() }
}

struct S {
  @ValidBuilder1 var v1: Int { 1 }
  @ValidBuilder2 var v2: Int { 1 }
  @ValidBuilder3 var v3: Int { 1 }
  @ValidBuilder4 var v4: Int { 1 }
  @ValidBuilder5 func v5() -> Int {}
  @InvalidBuilder1 var i1: Int { 1 } // expected-error {{result builder 'InvalidBuilder1' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder2 var i2: Int { 1 } // expected-error {{result builder 'InvalidBuilder2' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder3 var i3: Int { 1 } // expected-error {{result builder 'InvalidBuilder3' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder4 var i4: Int { 1 } // expected-error {{result builder 'InvalidBuilder4' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder5 var i5: Int { 1 } // expected-error {{result builder 'InvalidBuilder5' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder6 var i6: Int { 1 } // expected-error {{result builder 'InvalidBuilder6' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder7 var i7: Int { 1 } // expected-error {{result builder 'InvalidBuilder7' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder8 var i8: Int { 1 } // expected-error {{result builder 'InvalidBuilder8' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder9 var i9: Int { 1 } // expected-error {{result builder 'InvalidBuilder9' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder10 var i10: Int { 1 } // expected-error {{result builder 'InvalidBuilder10' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @InvalidBuilder11 var i11: InvalidBuilder11 { 1 } // expected-error {{result builder 'InvalidBuilder11' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
  @ValidPairwiseBuilder1 var i12: Int { 1 }
  @UnavailablePairwiseBuilder var i13: Int { 1; 1; 1 } // expected-error {{result builder 'UnavailablePairwiseBuilder' does not implement any 'buildBlock' or a combination of 'buildPartialBlock(first:)' and 'buildPartialBlock(accumulated:next:)' with sufficient availability for this call site}}
}
