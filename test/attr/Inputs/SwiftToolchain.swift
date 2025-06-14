// CHECK: @available(_SwiftToolchain 5.0)
// CHECK-NEXT: func fiveOnly() -> Int
@available(_SwiftToolchain, introduced: 5.0)
public func fiveOnly() -> Int {
  return 4
}

// CHECK: @available(_SwiftToolchain 5.0, macOS 10.1, *)
// CHECK-NEXT: func fiveOnlyWithMac() -> Int
@available(_SwiftToolchain, introduced: 5.0)
@available(macOS, introduced: 10.1)
public func fiveOnlyWithMac() -> Int {
  return 4
}

// CHECK: @available(macOS 10.1, _SwiftToolchain 5.0, *)
// CHECK-NEXT: func fiveOnlyWithMac2() -> Int
@available(macOS, introduced: 10.1)
@available(_SwiftToolchain, introduced: 5.0)
public func fiveOnlyWithMac2() -> Int {
  return 4
}

// CHECK: @available(_SwiftToolchain, introduced: 4.0, obsoleted: 5.0)
// CHECK-NEXT: func fourOnly() -> Int
@available(_SwiftToolchain, introduced: 4.0, obsoleted: 5.0)
public func fourOnly() -> Int {
  return 3
}

