// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// REQUIRES: swift_feature_CxxImplementation
// REQUIRES: swift_feature_ParserASTGen

@cxx
func foo() {}

@cxx(name: "cxx_bar")
func bar() {}
