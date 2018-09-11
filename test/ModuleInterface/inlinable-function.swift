// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-interface-path %t/Test.swiftinterface -module-name Test %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -emit-interface-path - -module-name Test | %FileCheck %s

// CHECK: public struct Foo : Hashable {
public struct Foo: Hashable {
  // CHECK: public var inlinableGetPublicSet: [[INT:(Swift.)?Int]] {
  public var inlinableGetPublicSet: Int {
  // CHECK: @inlinable get {
  // CHECK-NEXT: return 3
  // CHECK-NEXT: }
    @inlinable
    get {
      return 3
    }
    // CHECK-NEXT: set[[NEWVALUE:(\(newValue\))?]]{{$}}
    set {
      print("I am set to \(newValue)")
    }
  }

  // CHECK: public var noAccessors: [[INT]]{{$}}
  public var noAccessors: Int

  // CHECK: public var hasDidSet: [[INT]] {
  public var hasDidSet: Int {
    // CHECK-NEXT: @_transparent get{{$}}
    // CHECK-NEXT: set[[NEWVALUE]]{{$}}
    // CHECK-NOT: didSet
    didSet {
      print("b set to \(hasDidSet)")
    }
    // CHECK-NEXT: }
  }


  // CHECK: @_transparent public var transparent: [[INT]] {
  // CHECK:   return 34
  // CHECK: }
  @_transparent
  public var transparent: Int {
    return 34
  }

  // CHECK: public var transparentSet: [[INT]] {
  public var transparentSet: Int {
    // CHECK-NEXT: get{{$}}
    get {
      return 34
    }
    // CHECK-NEXT: @_transparent set[[NEWVALUE]] {
    // CHECK-NOT:   #if false
    // CHECK-NOT:   print("I should not appear")
    // CHECK-NOT:   #else
    // CHECK-NOT:   #if false
    // CHECK-NOT:   print("I also should not")
    // CHECK-NOT:   #else
    // CHECK:       print("I am set to \(newValue)")
    // CHECK-NOT:   #endif
    // CHECK-NOT:   #endif
    @_transparent
    set {
      #if false
      print("I should not appear")
      #else
      #if false
      print("I also should not")
      #else
      print("I am set to \(newValue)")
      #endif
      #endif
    }
  }

  // CHECK: @inlinable public var inlinableProperty: [[INT]] {
  @inlinable
  public var inlinableProperty: Int {
    // CHECK: get {
    // CHECK:   return 32
    // CHECK: }
    get {
      return 32
    }

    // CHECK: set[[NEWVALUE]] {
    // CHECK-NOT: #if true
    // CHECK:     print("I am set to \(newValue)")
    // CHECK-NOT: #else
    // CHECK-NOT: print("I should not appear")
    // CHECK-NOT  #endif
    // CHECK: }
    set {
      #if true
      print("I am set to \(newValue)")
      #else
      print("I should not appear")
      #endif
    }
  }

  // CHECK: @inlinable public func inlinableMethod() {
  // CHECK-NOT: #if NO
  // CHECK-NOT: print("Hello, world!")
  // CHECK-NOT: #endif
  // CHECK:     print("Goodbye, world!")
  // CHECK-NEXT: }
  @inlinable
  public func inlinableMethod() {
    #if NO
    print("Hello, world!")
    #endif
    print("Goodbye, world!")
  }


  // CHECK: @_transparent [[ATTRS:(mutating public|public mutating)]] func transparentMethod() {
  // CHECK-NEXT:   inlinableProperty = 4
  // CHECK-NEXT: }
  @_transparent
  mutating public func transparentMethod() {
    inlinableProperty = 4
  }

  // CHECK: @inline(__always) [[ATTRS]] func inlineAlwaysMethod() {
  // CHECK-NEXT: inlinableProperty = 4
  // CHECK-NEXT: }
  @inline(__always)
  mutating public func inlineAlwaysMethod() {
    inlinableProperty = 4
  }

  // CHECK: public func nonInlinableMethod(){{$}}
  // CHECK-NOT: print("Not inlinable")
  public func nonInlinableMethod() {
    print("Not inlinable")
  }

  // CHECK: {{^}}}
}

// CHECK-NOT: private func topLevelPrivate()
private func topLevelPrivate() {
  print("Ssshhhhh")
}

// CHECK: internal func topLevelUsableFromInline(){{$}}
@usableFromInline
internal func topLevelUsableFromInline() {
  topLevelPrivate()
}

// CHECK: @inlinable public func topLevelInlinable() {
// CHECK-NEXT:  topLevelUsableFromInline()
// CHECK-NEXT: }
@inlinable public func topLevelInlinable() {
  topLevelUsableFromInline()
}
