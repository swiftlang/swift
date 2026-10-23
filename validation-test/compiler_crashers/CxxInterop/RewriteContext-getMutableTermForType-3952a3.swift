// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"a1296288","signature":"swift::rewriting::RewriteContext::getMutableTermForType(swift::CanType, swift::ProtocolDecl const*)","signatureNext":"RequirementMachine::getReducedTypeParameter","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<b> {
  case (b, [a<Self>])
}
