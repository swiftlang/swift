// {"kind":"typecheck","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b { extension a!
