// REQUIRES: syntax_parser_lib
// RUN: %swift-syntax-parser-test %s -dump-diags 2>&1 | %FileCheck %s

// CHECK: [[@LINE+2]]:11 Error: consecutive statements on a line must be separated by ';'
// CHECK-NEXT: ([[@LINE+1]]:11,[[@LINE+1]]:11) Fixit: ";"
let number⁚ Int
// CHECK-NEXT: [[@LINE-1]]:11 Error: operator with postfix spacing cannot start a subexpression

// CHECK-NEXT: [[@LINE+2]]:3 Error: invalid character in source file
// CHECK-NEXT: ([[@LINE+1]]:3,[[@LINE+1]]:6) Fixit: " "
5 ‒ 5
// CHECK-NEXT: [[@LINE-1]]:3 Note: unicode character '‒' (Figure Dash) looks similar to '-' (Hyphen Minus); did you mean to use '-' (Hyphen Minus)?
// CHECK-NEXT: ([[@LINE-2]]:3,[[@LINE-2]]:6) Fixit: "-"
// CHECK-NEXT: [[@LINE-3]]:2 Error: consecutive statements on a line must be separated by ';'
// CHECK-NEXT: ([[@LINE-4]]:2,[[@LINE-4]]:2) Fixit: ";"

// CHECK-NEXT: [[@LINE+2]]:10 Error: expected ',' separator
// CHECK-NEXT: ([[@LINE+1]]:9,[[@LINE+1]]:9) Fixit: ","
if (true ꝸꝸꝸ false) {}

if (5 ‒ 5) == 0 {}
// CHECK-NEXT: [[@LINE-1]]:7 Error: invalid character in source file
// CHECK-NEXT: ([[@LINE-2]]:7,[[@LINE-2]]:10) Fixit: " "
// CHECK-NEXT: [[@LINE-3]]:7 Note: unicode character '‒' (Figure Dash) looks similar to '-' (Hyphen Minus); did you mean to use '-' (Hyphen Minus)?
// CHECK-NEXT: ([[@LINE-4]]:7,[[@LINE-4]]:10) Fixit: "-"
// CHECK-NEXT: [[@LINE-5]]:11 Error: expected ',' separator
// CHECK-NEXT: ([[@LINE-6]]:6,[[@LINE-6]]:6) Fixit: ","

let dateFormatter: DateFormatter = {
    let dateFormatter = DateFormatter()
    return dateFormatter
}()

// CHECK-NEXT: 7 error(s) 0 warnings(s) 2 note(s)