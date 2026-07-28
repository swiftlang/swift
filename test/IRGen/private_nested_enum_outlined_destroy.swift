// RUN: %target-swift-frontend -emit-ir %s -module-name test -enable-library-evolution -Onone | %FileCheck %s

// rdar://176512369: The outlined destroy of a type whose layout contains a
// file-private nested type (here Entry.Target) must use per-file `private`
// linkage rather than shared `linkonce_odr`.
//
// Only this file can access Target's metadata, so here the helper is emitted
// with an extra Target-metadata parameter and inlines the enum destroy using
// it. A different file cannot access Target, treats it as non-ABI-accessible,
// and emits a differently-shaped helper that falls back to the enclosing type's
// value witness. Sharing one mangled symbol between those two signatures let a
// caller pass Entry's struct metadata where Target's enum metadata was expected
// -> swift_getEnumCaseMultiPayload misread -> crash. Per-file private linkage
// keeps each file's copy self-consistent.

// CHECK: define private ptr @"$s4test5EntryVyxGSglWOh"(
// CHECK-SAME: %"Entry<Value>.Target"

// Conversely, when the nested type is only *internal* its metadata is
// accessible module-wide, so every file collects the same signature and the
// outlined destroy stays shared linkonce_odr.  This pins that the change above
// is scoped to file-private types.
// CHECK: define linkonce_odr hidden ptr @"$s4test9OpenEntryVyxGSglWOh"(
// CHECK-SAME: %"OpenEntry<Value>.Target"

public final class C1 { public init() {} }
public final class C2 { public init() {} }

public struct Entry<Value> {
  fileprivate enum Target {
    case a(C1, C2)
    case b(Value)
  }
  var value: Value
  fileprivate var target: Target
  public init(_ value: Value, _ c1: C1, _ c2: C2) {
    self.value = value
    self.target = .a(c1, c2)
  }
  public var first: C1? {
    switch target { case .a(let c, _): return c; default: return nil }
  }
}

public final class Location<Value> {
  var entries: [Entry<Value>]
  public init(_ e: [Entry<Value>]) { entries = e }
  // Destroys an Optional<Entry<Value>> in this file, forcing emission of the
  // outlined destroy helper whose linkage the CHECK above pins down.
  func firstFocused() -> Entry<Value>? {
    var m: Entry<Value>? = nil
    for e in entries { if e.first != nil { m = e; break } }
    return m
  }
}

// Control: same shape but with an *internal* (not file-private) nested enum.
public struct OpenEntry<Value> {
  enum Target {
    case a(C1, C2)
    case b(Value)
  }
  var value: Value
  var target: Target
  public init(_ value: Value, _ c1: C1, _ c2: C2) {
    self.value = value
    self.target = .a(c1, c2)
  }
  public var first: C1? {
    switch target { case .a(let c, _): return c; default: return nil }
  }
}

public final class OpenLocation<Value> {
  var entries: [OpenEntry<Value>]
  public init(_ e: [OpenEntry<Value>]) { entries = e }
  func firstFocused() -> OpenEntry<Value>? {
    var m: OpenEntry<Value>? = nil
    for e in entries { if e.first != nil { m = e; break } }
    return m
  }
}
