// {"kind":"typecheck","original":"b3a878fa","signature":"clang::Type const* swift::ClangTypeConverter::getFunctionType<false>(llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::Type, swift::FunctionTypeRepresentation)","signatureNext":"ClangTypeConverter::visitFunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a = @convention(thin) () -> Void
b = @convention(block) (a) -> Void
