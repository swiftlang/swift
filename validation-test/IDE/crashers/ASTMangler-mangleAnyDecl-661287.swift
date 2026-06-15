// {"kind":"complete","original":"7551d07a","signature":"swift::Mangle::ASTMangler::mangleAnyDecl(swift::ValueDecl const*, bool)","signatureNext":"Mangle::ASTMangler::mangleDeclWithPrefix"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  protocol b {
    struct c<d> {
      e{
      #^^#}
      f
      let g = f
    }
  }
}
