// CHECK: @available(swift 5.0)
// CHECK-NEXT: func fiveOnly() -> Int
@available(swift, introduced: 5.0)
public func fiveOnly() -> Int {
  return 4
}

// CHECK: @available(swift 5.0)
// CHECK-NEXT: @available(OSX 10.1, *)
// CHECK-NEXT: func fiveOnlyWithMac() -> Int
@available(swift, introduced: 5.0)
@available(macOS, introduced: 10.1)
public func fiveOnlyWithMac() -> Int {
  return 4
}

// CHECK: @available(swift 5.0)
// CHECK-NEXT: @available(OSX 10.1, *)
// CHECK-NEXT: func fiveOnlyWithMac2() -> Int
@available(macOS, introduced: 10.1)
@available(swift, introduced: 5.0)
public func fiveOnlyWithMac2() -> Int {
  return 4
}

// CHECK: @available(swift, introduced: 4.0, obsoleted: 5.0)
// CHECK-NEXT: func fourOnly() -> Int
@available(swift, introduced: 4.0, obsoleted: 5.0)
public func fourOnly() -> Int {
  return 3
}
