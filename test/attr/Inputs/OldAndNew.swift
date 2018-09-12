// CHECK: @available(swift 4.0)
// CHECK-NEXT: func fourOnly() -> Int
@available(swift, introduced: 4.0)
public func fourOnly() -> Int {
  return 4
}

// CHECK: @available(swift 4.0)
// CHECK-NEXT: @available(OSX 10.1, *)
// CHECK-NEXT: func fourOnlyWithMac() -> Int
@available(swift, introduced: 4.0)
@available(macOS, introduced: 10.1)
public func fourOnlyWithMac() -> Int {
  return 4
}

// CHECK: @available(swift 4.0)
// CHECK-NEXT: @available(OSX 10.1, *)
// CHECK-NEXT: func fourOnlyWithMac2() -> Int
@available(macOS, introduced: 10.1)
@available(swift, introduced: 4.0)
public func fourOnlyWithMac2() -> Int {
  return 4
}

// CHECK: @available(swift, introduced: 3.0, obsoleted: 4.0)
// CHECK-NEXT: func threeOnly() -> Int
@available(swift, introduced: 3.0, obsoleted: 4.0)
public func threeOnly() -> Int {
  return 3
}
