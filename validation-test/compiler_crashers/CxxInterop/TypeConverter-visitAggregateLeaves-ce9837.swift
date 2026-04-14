// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0316a04a","signature":"swift::Lowering::TypeConverter::visitAggregateLeaves(swift::Lowering::AbstractionPattern, swift::CanType, swift::TypeExpansionContext, std::__1::function<bool (swift::CanType, swift::Lowering::AbstractionPattern, swift::ValueDecl*, std::__1::optional<unsigned int>)>, std::__1::function<bool (swift::CanType, swift::Lowering::AbstractionPattern, swift::ValueDecl*, std::__1::optional<unsigned int>)>)","signatureAssert":"Assertion failed: (isTopLevel && \"aggregate containing marker type!?\"), function operator()","signatureNext":"Lowering::TypeConverter::verifyTrivialLowering"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
// REQUIRES: OS=macosx
import Foundation
struct a {
  var b = Foundation
}
