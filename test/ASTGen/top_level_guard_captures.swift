// A top-level 'guard' introduces bindings that the rest of the file can use.
// ASTGen did not mark the following declarations as possibly capturing them, so
// SILGen skipped their capture lists and then hit "not a global variable!" on the
// reference. Both parsers must produce a capture, not a global access.

// RUN: %target-swift-emit-silgen %s -enable-experimental-feature ParserASTGen \
// RUN:   | %FileCheck %s
// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Also exercise the SILGen path that actually asserted.
// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values -o /dev/null %s \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values -o /dev/null %s

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

guard let x: Int = nil else { while true { } }

// The capture shows up as a parameter rather than a global access.
// CHECK-LABEL: sil hidden [ossa] @$s24top_level_guard_captures0D1XyyF : $@convention(thin) (Int) -> () {
func capturesX() {
  _ = x
}

// CHECK-LABEL: sil hidden [ossa] @$s24top_level_guard_captures17transitiveCaptureyyF : $@convention(thin) (Int) -> () {
func transitiveCapture() {
  capturesX()
}
