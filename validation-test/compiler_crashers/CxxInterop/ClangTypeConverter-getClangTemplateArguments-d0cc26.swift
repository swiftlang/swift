// {"kind":"typecheck","signature":"swift::ClangTypeConverter::getClangTemplateArguments(clang::TemplateParameterList const*, llvm::ArrayRef<swift::Type>, llvm::SmallVectorImpl<clang::TemplateArgument>&)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"ASTContext::getClangTemplateArguments"}
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -I %S/Inputs/ClangTypeConverter-getClangTemplateArguments-d0cc26 %s

import ValueTemplate
let _ = S<CInt, CInt>()
