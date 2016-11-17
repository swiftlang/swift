// REQUIRES: objc_interop
// RUN: %target-swift-ide-test -syntax-coloring -playground -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -playground -typecheck -source-filename %s | %FileCheck %s

//: playground doc comment line
func playgroundCommentLine(f : () throws -> ()) rethrows {}
// CHECK: <doc-comment-line>//: playground doc comment line

/*:
  playground doc comment multi-line
  - returns: sdf
*/
func playgroundCommentMultiLine(f : () throws -> ()) rethrows {}
// CHECK: <doc-comment-block>/*:
// CHECK: playground doc comment multi-line
// CHECK:  - <doc-comment-field>returns</doc-comment-field>: sdf
// CHECK: */</doc-comment-block>

