// {"kind":"complete","signature":"swift::Mangle::Mangler::finalize(llvm::raw_ostream&)"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
struct a { extension b { struct c { struct d<e{ f { d#^COMPLETE^#
