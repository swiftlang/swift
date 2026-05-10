// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s -import-objc-header %S/Inputs/objc_generics_upper_bound.h

// REQUIRES: objc_interop

func upperBound(derived: Derived) -> Base<AnyObject> {
  return derived
}

func upperBound(derived: DerivedWithProtocol) -> BaseWithProtocol<any P> {
  return derived
}

func upperBound(derived: DerivedWithClass) -> BaseWithClass<C> {
  return derived
}

func upperBound(derived: DerivedWithBoth) -> BaseWithBoth<any C & P> {
  return derived
}
