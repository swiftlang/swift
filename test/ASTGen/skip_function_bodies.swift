// Function bodies are built on demand: a body that is skipped is never built,
// so nothing inside it is diagnosed.

// RUN: %empty-directory(%t)

// Everything is diagnosed when bodies are built.
// RUN: not %target-swift-frontend -typecheck -enable-experimental-feature ParserASTGen -diagnostic-style=llvm %s 2>&1 | %FileCheck %s --check-prefix=BUILT

// Nothing inside a skipped body is.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ParserASTGen -experimental-skip-all-function-bodies %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

struct S {
  func method() {
    let x = 1 +  // syntax error
  }
  init() { undefinedInInit() }
  var computed: Int {
    undefinedInGetter()
    return 0
  }
  var explicit: Int {
    get { undefinedInExplicitGetter() ; return 0 }
  }
}

class C {
  deinit { undefinedInDeinit() }
}

func free() { undefinedInFree() }

// BUILT-DAG: error: expected expression after operator
// BUILT-DAG: error: cannot find 'undefinedInInit' in scope
// BUILT-DAG: error: cannot find 'undefinedInDeinit' in scope
// BUILT-DAG: error: cannot find 'undefinedInGetter' in scope
// BUILT-DAG: error: cannot find 'undefinedInExplicitGetter' in scope
// BUILT-DAG: error: cannot find 'undefinedInFree' in scope
