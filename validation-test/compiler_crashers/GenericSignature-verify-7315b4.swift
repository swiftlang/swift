// {"kind":"typecheck","signature":"swift::GenericSignature::verify(llvm::ArrayRef<swift::Requirement>) const","signatureNext":"DeclChecker::visit"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b where Self : a protocol a
