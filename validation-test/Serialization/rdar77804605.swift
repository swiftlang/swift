// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/WrappedParameter.swiftmodule -emit-module-source-info-path %t/WrappedParameter.swiftsourceinfo -module-name WrappedParameter -enable-testing %s
// RUN: %target-swift-frontend -merge-modules -emit-module %t/WrappedParameter.swiftmodule -module-name WrappedParameter -o %t/WrappedParameter.swiftmodule

// Make sure wrapped parameters don't crash in merge-modules when
// they were compiled with -emit-module-source-info and -enable-testing.

@propertyWrapper
struct ProjectionWrapper<Value> {
  var wrappedValue: Value

  var projectedValue: Self { self }

  public init(projectedValue: Self) {
    self = projectedValue
  }
}

func test(@ProjectionWrapper value: Int) {}
