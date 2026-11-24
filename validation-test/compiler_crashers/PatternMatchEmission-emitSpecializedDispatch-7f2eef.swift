// {"kind":"emit-silgen","original":"b5b52396","signature":"(anonymous namespace)::PatternMatchEmission::emitSpecializedDispatch((anonymous namespace)::ClauseMatrix&, llvm::ArrayRef<swift::Lowering::ConsumableManagedValue>, unsigned int&, unsigned int, std::__1::function<void (swift::SILLocation)> const&)::$_1::operator()(swift::Pattern*, unsigned int) const","signatureAssert":"Assertion failed: (getSpecializingPattern(clauses[rowIndex][column]) == pattern), function operator()","signatureNext":"PatternMatchEmission::emitDispatch"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
}
func b(c: a) {
  switch c {
  case let d as a:
    0
  case is a:
    0
  }
}
