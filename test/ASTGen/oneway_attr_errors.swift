// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature SendCalls \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: concurrency
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_SendCalls

// rdar://116686158
// UNSUPPORTED: asan

// ASTGen keeps both of the contradicting '@oneway' and '@oneway(never)'
// attributes, which the type checker diagnoses like the C++ parser

// expected-error@+1{{'@oneway' contradicts previous attribute '@oneway(never)'}}
@MainActor @oneway(never) @oneway func both() {}

// expected-error@+1{{'@oneway(never)' contradicts previous attribute '@oneway'}}
@MainActor @oneway @oneway(never) func bothReversed() {}

// expected-error@+1{{expected 'never' in '@oneway' attribute}}
@oneway(always) func notAlways() {}

// '@oneway(never)' is a declaration attribute, so ASTGen does not know it as a
// type attribute
// expected-error@+1:21{{unknown attribute 'oneway'}}
typealias NeverFn = @oneway(never) () -> Void
