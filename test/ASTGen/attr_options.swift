// Option parsing for attributes that take a single identifier option, checked
// against both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution
// RUN: %target-typecheck-verify-swift -swift-version 5 -enable-library-evolution \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

@inline(sometimes) func f1() {}
// expected-error@-1 {{unknown option 'sometimes' for attribute 'inline'}}

@_optimize(fast) func f2() {}
// expected-error@-1 {{unknown option 'fast' for attribute '_optimize'}}

@_semantics("x") @exclusivity(maybe) var v1 = 0
// expected-error@-1 {{unknown option 'maybe' for attribute 'exclusivity'}}

@_spi(+) public func spi1() {}
// expected-error@-1 {{expected an SPI identifier as subject of the '@_spi' attribute}}

@_spi(set) public func spi2() {}
// expected-error@-1 {{expected an SPI identifier as subject of the '@_spi' attribute}}

@_spi(🤔) public func spi3() {}
@_spi(_) public func spi4() {}

func g(_ x: @isolated(foo) () -> Void) {}
// expected-error@-1 {{expected 'any' as the isolation kind}} {{23-26=any}}
