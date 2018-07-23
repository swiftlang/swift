/*comment*/`init`()
foo();/*comment*/`var`()

// RUN: not --crash %target-swift-ide-test -syntax-coloring -source-filename %s
// REQUIRES: asserts
