// RUN: %target-swift-ide-test -syntax-coloring -playground -source-filename %s | FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -playground -typecheck -source-filename %s | FileCheck %s

//: playground doc comment line
func playgroundCommentLine(f : () throws -> ()) rethrows {}
// CHECK: <doc-comment-line>//: playground doc comment line

// Keep this as the last test
/**
  Trailing off ...
func unterminatedBlockComment() {}
// CHECK: <comment-line>// Keep this as the last test</comment-line>
// CHECK: <doc-comment-block>/**
// CHECK:  Trailing off ...
// CHECK:  func unterminatedBlockComment() {}
// CHECK:  </doc-comment-block>
