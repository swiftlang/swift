// {"kind":"typecheck","signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (0 && \"Bad base type\"), function getContextSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a< b {
  @propertyWrapper struct c {
    var wrappedValue
        projectedValue : a
                                        init(projectedValue
                                             a) {
      func d(@c b) d($e
                                                            : f
