// {"kind":"complete","signature":"swift::Mangle::Mangler::verify(llvm::StringRef, swift::Mangle::ManglingFlavor)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  fileprivate associatedtype b
  func c ->
  #^COMPLETE^#
