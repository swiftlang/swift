// '#file' means '#filePath' unless ConciseMagicFile is on, and the call that
// appends an interpolation is located at the '(' of its segment.

// RUN: %target-swift-emit-silgen -module-name magic %s -Xllvm -sil-print-debuginfo \
// RUN:   -enable-experimental-feature ParserASTGen | %FileCheck %s --check-prefix=CHECK-BOTH
// RUN: %target-swift-emit-silgen -module-name magic %s -Xllvm -sil-print-debuginfo \
// RUN:   | %FileCheck %s --check-prefix=CHECK-BOTH

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func file() -> String { #file }
// CHECK-BOTH-LABEL: sil hidden [ossa] @$s5magic4fileSSyF
// CHECK-BOTH: string_literal utf8 "{{.*}}magic_file_and_interpolation.swift"
// CHECK-BOTH-NOT: "magic/magic_file_and_interpolation.swift"

func interpolated(_ x: Int) -> String { "a \(x) b" }
// CHECK-BOTH-LABEL: sil hidden [ossa] @$s5magic12interpolatedySSSiF
// CHECK-BOTH: function_ref @$ss26DefaultStringInterpolationV06appendC0{{.*}}loc "{{.*}}":[[@LINE-2]]:45
