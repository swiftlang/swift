// {"kind":"complete","noObjCInterop":true,"original":"aad610a6","signature":"(anonymous namespace)::SwiftDeclConverter::getImplicitProperty(swift::importer::ImportedName, clang::FunctionDecl const*)","signatureAssert":"Assertion failed: (foundAccessor && \"Didn't find the original accessor? \" \"Try clearing your module cache\"), function getImplicitProperty","signatureNext":"SwiftDeclConverter::VisitFunctionDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -disable-objc-interop -source-filename %s
// REQUIRES: OS=macosx
import Dispatch
#^^#
