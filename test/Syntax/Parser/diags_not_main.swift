// REQUIRES: syntax_parser_lib
// RUN: %swift-syntax-parser-test %s -dump-diags -main=false 2>&1 | %FileCheck %s

_ = 1 + 1
// CHECK: [[@LINE-1]]:1 Error: expressions are not allowed at the top level
// CHECK-NEXT: 1 error(s) 0 warnings(s) 0 note(s)
