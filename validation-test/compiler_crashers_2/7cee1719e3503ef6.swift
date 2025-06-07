// {"signature":"swift::GenericParamKey::findIndexIn(llvm::ArrayRef<swift::GenericTypeParamType*>) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: rdar152763265
a[[[[ a a?[[a a?
