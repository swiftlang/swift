// {"kind":"complete","original":"588b7eea","signature":"swift::NominalType::get(swift::NominalTypeDecl*, swift::Type, llvm::ArrayRef<swift::Type>, swift::ASTContext const&, llvm::ArrayRef<swift::ScopeRef>)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"getSelectedOverloadInfo"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
import Foundation
Foundation[
  #^^#]
