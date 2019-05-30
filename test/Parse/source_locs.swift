// Check source locations (only InterpolatedStringLiteral for now).

func string_interpolation() {
  "\("abc")"
}

// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s
// CHECK: (interpolated_string_literal_expr {{.*}} trailing_quote_loc=SOURCE_DIR{{/|\\}}test{{/|\\}}Parse{{/|\\}}source_locs.swift:4:12 {{.*}}
