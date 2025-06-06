// {"signature":"swift::GenericParamKey::findIndexIn(llvm::ArrayRef<swift::GenericTypeParamType*>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a[[[[ a a?[[a a?
