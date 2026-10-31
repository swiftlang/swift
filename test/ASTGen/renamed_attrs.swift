// Old spellings of attributes are diagnosed the way the C++ parser does, so
// the same expectations hold for both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

@_inlineable public func f1() {}
// expected-error@-1 {{'@_inlineable' has been renamed to '@inlinable'}}{{2-13=inlinable}}

@_versioned func f2() {}
// expected-error@-1 {{'@_versioned' has been renamed to '@usableFromInline'}}{{2-12=usableFromInline}}

@inlineable public func f3() {}
// expected-error@-1 {{no attribute named '@inlineable'; did you mean '@inlinable'?}}{{2-12=inlinable}}

@_functionBuilder struct B { static func buildBlock() {} }
// expected-warning@-1 {{'@_functionBuilder' has been renamed to '@resultBuilder'}}{{2-18=resultBuilder}}

func g(@_unsafeSendable _ body: @escaping () -> Void) {}
// expected-warning@-1 {{'_unsafeSendable' attribute has been removed in favor of '@preconcurrency'}}{{8-24=}}
