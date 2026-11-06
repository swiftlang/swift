// Unterminated brace-delimited constructs, whose closing '}' the parser has to
// synthesize. A synthesized '}' has a zero-width position just past the previous
// token and its trailing trivia, so using it makes a construct's range exceed
// its own contents -- and sometimes reach into the next declaration -- which
// ASTScope and the availability-scope builder reject.
//
// Neither parser can produce a useful AST here; what matters is that both reject
// the file rather than aborting on an internal invariant.

// RUN: not %target-swift-frontend -typecheck -parse-as-library %s \
// RUN:   -enable-experimental-feature CoroutineAccessors \
// RUN:   -enable-experimental-feature ParserASTGen 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -parse-as-library %s \
// RUN:   -enable-experimental-feature CoroutineAccessors 2>&1 | %FileCheck %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_CoroutineAccessors

// An accessor block that is never closed used to give the variable a range
// running into the declaration after it.
// CHECK-DAG: error:
var unterminatedAccessor: Int {
  read {
  yielding borrow {
  }
}

var afterIt: Int { 0 }

// A closure that is never closed used to leave its parameter scope outside the
// enclosing brace statement.
func unterminatedClosure() {
  someUndefinedFunction {
}

// An 'else' with no body at all: neither brace is present and there are no
// contents, so there is no token to end the range at. Collapsing to the
// construct's start keeps the range a valid zero-width point -- a range with one
// valid and one invalid endpoint is itself an assertion failure.
func elseWithoutBody() {
  if someUndefinedCondition() {
  } else
}

// A declaration whose braces are missing entirely, rather than just unclosed.
// The range has to end at the start of a token *inside* the declaration:
// getCharSourceRangeOfScope extends a scope's end with getLocForEndOfToken, so an
// end sitting on trailing trivia lexes forward into the next declaration and
// overlaps it.
func noParameterClause
func following(x: Int) {}

// A brace-less member block still has to contain whatever swift-syntax's recovery
// absorbed into it -- a bare 'extension' swallows the extensions after it -- or
// the members fall outside their own parent.
extension

extension SomeUndefinedType {

extension SomeOtherUndefinedType
