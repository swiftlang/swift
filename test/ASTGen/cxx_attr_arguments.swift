// Malformed arguments to '@cxx' are rejected by both parsers.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop
// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   -disable-objc-interop

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_CxxImplementation

// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx()
func emptyParens() {}

// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx("cxx_foo")
func stringName() {}

// expected-error@+1{{expected C++ function name in 'cxx' attribute}}
@cxx(name: "cxx_foo")
func labeledName() {}
