// {"kind":"complete","original":"ea03c605","signature":"swift::Mangle::ASTMangler::appendConstrainedExistential(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureNext":"Mangle::ASTMangler::appendType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b, c {
  associatedtype b
  associatedtype c
}
protocol d<b, c>: a
  func e {
    #^^#
  }
  func f
  :
    a<Int, String>
  &
    d<Int, String>
