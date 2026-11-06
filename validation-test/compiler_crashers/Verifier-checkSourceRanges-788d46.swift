// {"kind":"typecheck","original":"441eb0b8","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)","signatureNext":"Verifier::dispatchVisitPost"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
"""
registers \(each as separator * transform { String( { $0
radix
6 ) })
"""})
"""
