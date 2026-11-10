// RUN: not %target-swift-frontend -debug-diagnostic-names -disable-experimental-feature ParserASTGen -localization-path %S/Inputs -locale en -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK_NAMES
// These tests use lexer/parser diagnostics, which ASTGen does not localize.

_ = "HI!
// CHECK_NAMES: error: unterminated string literal [lex_unterminated_string]{{$}}

struct Circular { let x = x }
// CHECK_NAMES: error: circular reference [circular_reference]{{$}}
// CHECK_NAMES: note: through reference here [circular_reference_through]{{$}}

struct Broken {
  var b : Bool = True 
}
// CHECK_NAMES: error: cannot find 'True' in scope [cannot_find_in_scope]{{$}}
