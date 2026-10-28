// {"kind":"emit-sil","signature":"swift::TypeTreeLeafTypeRange::setBits(llvm::SmallBitVector&) const","signatureAssert":"Assertion failed: (E <= size() && \"Attempted to set out-of-bounds range!\"), function set","signatureNext":"UseState::setAffectedBits"}
// RUN: not --crash %target-swift-frontend -emit-sil -cxx-interoperability-mode=default -I %S/Inputs/TypeTreeLeafTypeRange-setBits-103961 %s

import NonCopyableDerived

let p = NonCopyableDerived()
_ = p.second
