// {"kind":"complete","signature":"openTypeParameter(swift::constraints::ConstraintSystem&, swift::Type, swift::GenericEnvironment*, llvm::DenseMap<swift::SubstitutableType*, swift::TypeVariableType*, llvm::DenseMapInfo<swift::SubstitutableType*, void>, llvm::detail::DenseMapPair<swift::SubstitutableType*, swift::TypeVariableType*>>&)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b:RangeReplaceableCollection where b ={ c: b.Element? { #^COMPLETE^#
