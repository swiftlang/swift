// {"kind":"typecheck","original":"64bf42f5","signature":"(anonymous namespace)::TypeResolver::resolveGenericArguments(swift::ValueDecl*, swift::GenericContext const*, swift::DeclRefTypeRepr*, swift::TypeResolutionOptions, llvm::SmallVectorImpl<swift::Type>&)","signatureAssert":"Assertion failed: (found != matcher.pairs.end()), function resolveGenericArguments","signatureNext":"TypeResolver::applyGenericArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b, each c, each d> = a<b, repeat each c, repeat each d>
