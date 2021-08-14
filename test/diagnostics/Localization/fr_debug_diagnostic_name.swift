// RUN: %empty-directory(%t)
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/fr.yaml --output-directory=%t/
// RUN: swift-serialize-diagnostics --input-file-path=%S/Inputs/en.yaml --output-directory=%t/
// RUN: not %target-swift-frontend -debug-diagnostic-names -localization-path %S/Inputs -locale fr -typecheck %s 2>&1 | %FileCheck %s --check-prefix=CHECK_NAMES

_ = "HI!
// CHECK_NAMES: error: chaîne non terminée littérale [lex_unterminated_string]{{$}}

// FIXME: This used to produce a localized diagnostic.

var self1 = self1
// CHECK_NAMES: error: circular reference [circular_reference]{{$}}
// CHECK_NAMES: note: through reference here [circular_reference_through]{{$}}

struct Broken {
  var b : Bool = True
}
// CHECK_NAMES: error: impossible de trouver 'True' portée [cannot_find_in_scope]{{$}}
