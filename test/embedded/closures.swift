// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public func foo<Result>(_ body: () -> Result) -> Result {
  return body()
}

public func main() {
    foo() { }
}
