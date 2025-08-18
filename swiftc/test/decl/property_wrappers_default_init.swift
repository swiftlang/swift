// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s

@propertyWrapper
struct Wrapper {
  init() {}

  var wrappedValue: Int = 0
}

// CHECK-LABEL: internal struct UseWrapperDefaultInit
struct UseWrapperDefaultInit {
  @Wrapper var value
  // CHECK: internal init(value: Wrapper = Wrapper())
}

let _ = UseWrapperDefaultInit()

