// {"kind":"complete","signature":"swift::NominalTypeDecl::prepareConformanceTable() const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
protocol a< b {
  associatedtype b
  func c<d>: a<d>#^COMPLETE^#
