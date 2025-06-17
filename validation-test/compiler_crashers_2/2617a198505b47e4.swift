// {"signature":"swift::rewriting::RequirementMachine::getReducedShape(swift::Type, llvm::ArrayRef<swift::GenericTypeParamType*>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{ b < c > (c, _ : c}
         protocol d : a{
  b<c : e>(c, c.c) protocol e {
    associatedtype c struct f : d
