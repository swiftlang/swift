// {"kind":"typecheck","signature":"swift::ClangSyntaxPrinter::printGenericRequirementInstantiantion(swift::GenericRequirement const&)","signatureAssert":"Assertion failed: (requirement.isAnyMetadata() && \"protocol requirements not supported yet!\"), function printGenericRequirementInstantiantion","signatureNext":"ClangSyntaxPrinter::printGenericRequirementsInstantiantions"}
// RUN: not --crash %target-swift-frontend -typecheck -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s

public protocol P {}
public struct S<T> {
  public var x: Int
}
extension S where T: P {
  public enum E {
    case a(Int)
  }
}
