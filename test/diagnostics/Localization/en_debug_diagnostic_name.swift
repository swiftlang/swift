// RUN: not %target-swift-frontend -debug-diagnostic-names -localization-path %S/Inputs -locale en -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK_NAMES

_ = "HI!
// CHECK_NAMES: error: unterminated string literal [lex_unterminated_string]{{$}}

var self1 = self1
// CHECK_NAMES: error: circular reference [circular_reference]{{$}}
// CHECK_NAMES: note: through reference here [circular_reference_through]{{$}}

struct Broken {
  var b : Bool = True 
}
// CHECK_NAMES: error: cannot find 'True' in scope [cannot_find_in_scope]{{$}}
