// Malformed expressions whose recovery used to build ASTs with ranges the AST
// verifier rejects. These are all errors, so the dumps are compared with 'not'.

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend-dump-parse -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: not %target-swift-frontend-dump-parse \
// RUN:    | %sanitize-address > %t/cpp-parser.ast
// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// A ternary missing its ':'. With no 'else' expression the TernaryExpr's range
// degenerates and no longer contains its children's.
// `Parser::parseExprSequence` fails the whole sequence, so the enclosing
// expression becomes an ErrorExpr.
func unbalancedTernary(a: Bool, b: Bool) {
  _ = (a ? b)
}

// A 'try' the parser synthesized during recovery. Its zero-width position sits
// past the preceding token *and its trailing trivia*, so a Try expression built
// from it can end up with an inverted range -- start after end. The trailing
// comment matters: it is what pushes the synthesized position further right.
func tryWithoutExpression() throws -> Int {
  try throw // a trailing comment, to move the synthesized position rightwards
  ;
  return 0
}

// An optional-chain suffix after a comment, where the child's range ran past the
// parent's.
func commentBeforeSuffix(foo: Int?) {
  _ = foo/* */?.description
}

// A statement body whose braces the parser had to synthesize. The synthesized
// positions land past the last token's trailing trivia -- outside the statement
// entirely, sometimes past the end of the line -- so a body built from them gave
// the statement a range extending beyond what it consumed, and the statement's
// own child scopes then fell outside it.
//
// Last in the file: a bare 'for' swallows whatever follows into its body during
// recovery, and the two parsers disagree about how much.
for i
