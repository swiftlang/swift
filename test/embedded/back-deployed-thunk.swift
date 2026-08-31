// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Osize -emit-sil -o /dev/null
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -Onone -emit-sil -o /dev/null

// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -emit-silgen -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// CHECK: sil{{.*}}[back_deployed_thunk]{{.*}}@$e4main3FooV3fooACvgZTwb

public struct Foo {
  public init() {}

  @backDeployed(before: macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0)
  public static var foo: Foo { Foo() }
}

@backDeployed(before: macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0)
public func backDeployedFunc() -> Int { 42 }

public func use() -> Int {
  _ = Foo.foo
  return backDeployedFunc()
}
