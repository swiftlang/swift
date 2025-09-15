// {"kind":"complete","signature":"swift::Mangle::Mangler::verify(llvm::StringRef, swift::Mangle::ManglingFlavor)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
protocol a {
  fileprivate associatedtype b
  func c ->
  #^COMPLETE^#
