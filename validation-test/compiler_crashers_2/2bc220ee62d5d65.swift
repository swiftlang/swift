// {"signature":"swift::Parser::parseTypeSimple(swift::Diag<>, swift::Parser::ParseTypeReason)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(a: ~b          <
