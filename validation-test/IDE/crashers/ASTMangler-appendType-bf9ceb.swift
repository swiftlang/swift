// {"kind":"complete","original":"eb66ea75","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureNext":"Mangle::ASTMangler::appendTupleTypeListElement"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
    ana<b>(
      b , c: b -> a
      d e  = ana(
        (depth        Swift) {
          f -> 0([
            #^^#,
            f,
