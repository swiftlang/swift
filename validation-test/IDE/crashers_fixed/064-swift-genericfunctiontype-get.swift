// We disable USR to Decl verification since the USR for declarations within the
// extension with no type name have a USR that looks like a top-level declaration.

// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s -code-completion-verify-usr-to-decl=false
extension{enum a<H{enum b{case
func a(=#^A^#
