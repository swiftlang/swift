// {"kind":"complete","original":"805c9f77","signature":"createImplicitConstructor(swift::NominalTypeDecl*, ImplicitConstructorKind, std::__1::optional<swift::MemberwiseInitKind>, swift::ASTContext&)","signatureAssert":"Assertion failed: (!decl->hasClangNode()), function createImplicitConstructor","signatureNext":"SynthesizeDefaultInitRequest::evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
import Foundation @implementation extension NSObject #^^#
