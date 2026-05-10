// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"11827281","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureNext":"Mangle::ASTMangler::appendDeclType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
// REQUIRES: OS=macosx
import Foundation
class a {
  var b = Foundation
}
