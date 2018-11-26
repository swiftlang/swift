// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll


// SR-6200: canonicalizing a conformance access path that was built
// without the requirement signature.
public struct Valid<V> {}

extension Valid where V: ValidationSuite {}

public protocol Validatable {}

extension Validatable {
    public func tested() {}

    // CHECK-LABEL: define{{.*}}$s23conformance_access_path11ValidatablePAAE6tested2byyqd__m_t9InputTypeQyd__RszAA15ValidationSuiteRd__lF
  public func tested<S: ValidationSuite>(by suite: S.Type) where S.InputType == Self {
      // CHECK:   [[S_AS_VALIDATION_SUITE_GEP:%[0-9]+]] = getelementptr inbounds i8*, i8** %S.ValidationSuite, i32 1
      // CHECK:   [[S_AS_VALIDATION_SUITE:%[0-9]+]] = load i8*, i8** [[S_AS_VALIDATION_SUITE_GEP]]
      // CHECK-NEXT:   [[S_VALIDATOR_BASE:%.*]] = bitcast i8* [[S_AS_VALIDATION_SUITE]] to i8**
      // CHECK-NEXT: [[S_VALIDATABLE_ADDR:%[0-9]+]] = getelementptr inbounds i8*, i8** [[S_VALIDATOR_BASE]], i32 1
      // CHECK-NEXT: [[S_VALIDATABLE_FN_RAW:%[0-9]+]] = load i8*, i8** [[S_VALIDATABLE_ADDR]]
      // CHECK-NEXT: [[S_VALIDATABLE_FN:%[0-9]+]] = bitcast i8* [[S_VALIDATABLE_FN_RAW]] to i8** (%swift.type*, %swift.type*, i8**)*
      // CHECK-NEXT: call swiftcc i8** [[S_VALIDATABLE_FN]](%swift.type* %Self, %swift.type* %S, i8** %S.Validator)
      tested()
    }
}

public protocol Validator {
    associatedtype InputType: Validatable
}

public protocol ValidationSuite: Validator {
    associatedtype InputType: Validatable
}
