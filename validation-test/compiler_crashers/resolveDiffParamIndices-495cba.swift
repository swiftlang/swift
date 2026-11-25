// {"kind":"typecheck","original":"09681508","signature":"resolveDiffParamIndices(swift::AbstractFunctionDecl*, swift::DifferentiableAttr*, swift::GenericSignature)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DifferentiableAttributeTypeCheckRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
import _Differentiation
{
  @differentiable() subscript() -> <#type#> {
    {
    }
  }
}
