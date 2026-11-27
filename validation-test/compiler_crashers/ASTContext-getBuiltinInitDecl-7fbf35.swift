// {"frontendArgs":["-typecheck","-parse-stdlib","-module-name","Swift","-import-objc-header","%t/m.h"],"kind":"custom","signature":"swift::ASTContext::getBuiltinInitDecl(swift::NominalTypeDecl*, swift::KnownProtocolKind, llvm::function_ref<swift::DeclName (swift::ASTContext&)>) const","signatureNext":"ASTContext::getIntBuiltinInitDecl"}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -parse-stdlib -module-name Swift -import-objc-header %t/m.h %t/main.swift

//--- m.h
enum E { A = 1 };

//--- main.swift
typealias CUnsignedInt = ()
let x = A
