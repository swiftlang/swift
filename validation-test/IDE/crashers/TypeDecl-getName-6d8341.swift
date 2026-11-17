// {"kind":"complete","original":"1d13cad6","signature":"swift::TypeDecl::getName() const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
  extension {
    enum b<c {
      case d
      static func e {
        #^^#
