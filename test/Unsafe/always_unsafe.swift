// RUN: %target-typecheck-verify-swift

// Uses of '@unsafe(always)' declarations must be acknowledged with 'unsafe'
// even though strict memory safety checking is not enabled here.

@unsafe(always) func alwaysUnsafeFunc() { }

@unsafe func unsafeFunc() { }

@unsafe(always)
struct AlwaysUnsafeType {
  init() { }
  func method() { }
}

// -----------------------------------------------------------------------
// Calls and references
// -----------------------------------------------------------------------
func testCalls() {
  alwaysUnsafeFunc()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeFunc()'}}

  unsafe alwaysUnsafeFunc()

  // A merely unsafe declaration doesn't need to be acknowledged when strict
  // memory safety checking is disabled.
  unsafeFunc()
  unsafe unsafeFunc()
}

func testTypeUses(value: AlwaysUnsafeType) {
  value.method()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{argument 'self' in call to instance method 'method' has unsafe type 'AlwaysUnsafeType'}}
  // expected-note@-3{{reference to parameter 'value' involves unsafe type 'AlwaysUnsafeType'}}

  unsafe value.method()
}

// Only the always-unsafe use is reported in this language mode.
@unsafe(always) func alwaysUnsafeValue() -> Int { 0 }

@unsafe func unsafeValue() -> Int { 0 }

func takesTwo(_: Int, _: Int) { }

func testMixed() {
  takesTwo(alwaysUnsafeValue(), unsafeValue())
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeValue()'}}

  unsafe takesTwo(alwaysUnsafeValue(), unsafeValue())
}

@safe func safeWrapper() {
  unsafe alwaysUnsafeFunc()
}

func testSafeWrapper() {
  safeWrapper()
}

// -----------------------------------------------------------------------
// Types that are only reachable through sugar or nesting
// -----------------------------------------------------------------------

typealias MaybeAlwaysUnsafe = AlwaysUnsafeType?

func testThroughTypealias(value: MaybeAlwaysUnsafe) {
  _ = value
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to parameter 'value' involves unsafe type 'AlwaysUnsafeType'}}

  _ = unsafe value
}

// Nesting doesn't inherit the strength; only the inner '@unsafe' applies.
@unsafe(always)
struct AlwaysUnsafeOuter {
  @unsafe struct MerelyUnsafeInner { init() { } }
}

func testNestedType(value: AlwaysUnsafeOuter.MerelyUnsafeInner) {
  _ = value
}

// -----------------------------------------------------------------------
// Generic substitutions
// -----------------------------------------------------------------------
func generic<T>(_: T) { }

func testGenericSubstitution() {
  generic(AlwaysUnsafeType())
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{argument #0 in call to global function 'generic' has unsafe type 'AlwaysUnsafeType'}}
  // expected-note@-3{{reference to unsafe type 'AlwaysUnsafeType'}}
  // expected-note@-4{{reference to initializer 'init()' involves unsafe type 'AlwaysUnsafeType'}}

  unsafe generic(AlwaysUnsafeType())
}

// -----------------------------------------------------------------------
// for-in loops
// -----------------------------------------------------------------------
struct AlwaysUnsafeIterator: @unsafe IteratorProtocol {
  @unsafe(always) mutating func next() -> Int? { nil }
}

struct SequenceWithAlwaysUnsafeIterator: Sequence {
  func makeIterator() -> AlwaysUnsafeIterator { AlwaysUnsafeIterator() }
}

func testForIn(s: SequenceWithAlwaysUnsafeIterator) {
  for _ in s { }
  // expected-error@-1{{for-in loop uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe instance method 'next()'}}

  for unsafe _ in s { }
}

// -----------------------------------------------------------------------
// Extensions
// -----------------------------------------------------------------------
struct SafeType { }

@unsafe(always)
extension SafeType {
  func viaAlwaysUnsafeExtension() { }
}

func testExtensionMember(value: SafeType) {
  value.viaAlwaysUnsafeExtension()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe instance method 'viaAlwaysUnsafeExtension()'}}

  unsafe value.viaAlwaysUnsafeExtension()
}

// -----------------------------------------------------------------------
// Enum elements
// -----------------------------------------------------------------------
enum HasAlwaysUnsafeCase {
  case safeCase
  @unsafe(always) case alwaysUnsafeCase
}

func testEnumElement() {
  _ = HasAlwaysUnsafeCase.alwaysUnsafeCase
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe enum case 'alwaysUnsafeCase'}}

  _ = unsafe HasAlwaysUnsafeCase.alwaysUnsafeCase
  _ = HasAlwaysUnsafeCase.safeCase
}

// -----------------------------------------------------------------------
// A plain '@unsafe' does not launder an always-unsafe signature
// -----------------------------------------------------------------------
@unsafe func merelyUnsafeReturningAlwaysUnsafe() -> AlwaysUnsafeType {
  unsafe AlwaysUnsafeType()
}

func testLaunderingThroughUnsafeDecl() {
  _ = merelyUnsafeReturningAlwaysUnsafe()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to global function 'merelyUnsafeReturningAlwaysUnsafe()' involves unsafe type 'AlwaysUnsafeType'}}

  _ = unsafe merelyUnsafeReturningAlwaysUnsafe()
}

// -----------------------------------------------------------------------
// An always-unsafe type wins over a merely unsafe one, wherever it appears
// -----------------------------------------------------------------------
@unsafe struct MerelyUnsafeType { init() { } }

struct Pair<T, U> { }

func merelyThenAlways() -> (MerelyUnsafeType, AlwaysUnsafeType) {
  (unsafe MerelyUnsafeType(), unsafe AlwaysUnsafeType())
}

func alwaysThenMerely() -> (AlwaysUnsafeType, MerelyUnsafeType) {
  (unsafe AlwaysUnsafeType(), unsafe MerelyUnsafeType())
}

func testTupleOrder() {
  _ = merelyThenAlways()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to global function 'merelyThenAlways()' involves unsafe type 'AlwaysUnsafeType'}}

  _ = alwaysThenMerely()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to global function 'alwaysThenMerely()' involves unsafe type 'AlwaysUnsafeType'}}

  _ = unsafe merelyThenAlways()
  _ = unsafe alwaysThenMerely()
}

func testGenericArgumentOrder(x: Pair<MerelyUnsafeType, AlwaysUnsafeType>,
                              y: Pair<AlwaysUnsafeType, MerelyUnsafeType>) {
  _ = x
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to parameter 'x' involves unsafe type 'AlwaysUnsafeType'}}

  _ = y
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to parameter 'y' involves unsafe type 'AlwaysUnsafeType'}}

  _ = unsafe x
  _ = unsafe y
}

// -----------------------------------------------------------------------
// Compiler-synthesized code
// -----------------------------------------------------------------------

// Synthesized code cannot be annotated with 'unsafe' by hand, so it is not
// diagnosed here.
@unsafe(always)
struct AlwaysUnsafeCodable: Codable {
  var value: Int
}

@unsafe(always)
enum AlwaysUnsafeRawRepresentable: Int {
  case one = 1
}

@propertyWrapper
@unsafe(always)
struct AlwaysUnsafeWrapper {
  var wrappedValue: Int
  init(wrappedValue: Int) { unsafe self.wrappedValue = wrappedValue }
}

struct UsesWrapper {
  @AlwaysUnsafeWrapper var value: Int = 0
}
