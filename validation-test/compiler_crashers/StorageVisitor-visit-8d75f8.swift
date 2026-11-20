// {"kind":"typecheck","original":"88a87e44","signature":"swift::StorageVisitor::visit(swift::NominalTypeDecl*, swift::DeclContext*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"checkInvertibleConformanceCommon"}
// RUN: not --crash %target-swift-frontend -typecheck %s
public
  protocol a
{
  struct b<c> {
    extension <#type#> {
      public
        enum d
      {
        case (c)
      }
    }
  }
}
