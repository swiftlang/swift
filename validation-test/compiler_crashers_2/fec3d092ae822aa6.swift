// {"signature":"swift::Parser::parseExprSequenceElement(swift::Diag<>, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
any a <
