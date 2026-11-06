// RUN: not %target-swift-frontend -diagnostic-style=swift -typecheck %/s 2>&1 | %FileCheck %s --check-prefixes=%if parser_astgen %{ASTGEN%} %else %{CPARSER%}

// REQUIRES: swift_swift_parser

// CPARSER:      {{SOURCE_DIR[/\]test[/\]diagnostics[/\]pretty-printed-diagnostics-eof\.swift}}:[[#LINE:]]:1: error: expected '}' in struct
// CPARSER:      [[#LINE-2]] | struct MyStruct {
// CPARSER-NEXT:             |                 `- note: to match this opening '{'
// CPARSER-NEXT: [[#LINE-1]] |   func foo() {}
// CPARSER-NEXT: [[#LINE]]   |
// CPARSER-NEXT:             | `- error: expected '}' in struct

// ASTGen reports the missing '}' at the end of the last token.
// ASTGEN:      {{SOURCE_DIR[/\]test[/\]diagnostics[/\]pretty-printed-diagnostics-eof\.swift}}:[[#LINE:]]:16: error: expected '}' to end struct
// ASTGEN:      [[#LINE-1]] | struct MyStruct {
// ASTGEN-NEXT:             |                 `- note: to match this opening '{'
// ASTGEN-NEXT: [[#LINE]]   |   func foo() {}
// ASTGEN-NEXT:             |                `- error: expected '}' to end struct

struct MyStruct {
  func foo() {}
