// Check the trailingQuoteLoc field of InterpolatedStringLiteral

"\("abc")"

// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s
// CHECK: (interpolated_string_literal_expr {{.*}} trailing_quote_loc={{.*}}:3:10 {{.*}}
