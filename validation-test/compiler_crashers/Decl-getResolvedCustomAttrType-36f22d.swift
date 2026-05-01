// {"kind":"typecheck","original":"30a0fcdc","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b<c> {
    @propertyWrapper struct d {
      extension String {
        struct e {
          @d var f: <#type#>
        }
      }
    }
  }
}
