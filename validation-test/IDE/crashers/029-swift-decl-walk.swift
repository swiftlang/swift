// RUN: not --crash %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s
// REQUIRES: asserts
// REQUIRES: swift_ast_verifier
{var f={{#^A^#}r
