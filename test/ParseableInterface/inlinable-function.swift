// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-parseable-module-interface-path %t/Test.swiftinterface -module-name Test %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-parseable-module-interface-path - -module-name Test | %FileCheck %s

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
    // CHECK-NEXT: {{^}}  }
  }

  // CHECK: public var noAccessors: [[INT]]{{$}}
  public var noAccessors: Int

  // CHECK: public var hasDidSet: [[INT]] {
  public var hasDidSet: Int {
    // CHECK-NEXT: didSet{{$}}
    didSet {
      print("b set to \(hasDidSet)")
    }
    // CHECK-NEXT: {{^}}  }
  }


  // CHECK: @_transparent public var transparent: [[INT]] {
  // CHECK-NEXT:   get {
  // CHECK-NEXT:   return 34
  // CHECK-NEXT: }
  // CHECK-NEXT: }
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
    // CHECK-NEXT: }
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
    // CHECK-NEXT: }
  }

  // CHECK: @inlinable public var inlinableReadAndModify: [[INT]] {
  @inlinable
  public var inlinableReadAndModify: Int {
    // CHECK: _read {
    // CHECK-NEXT: yield 0
    // CHECK-NEXT: }
    _read {
      yield 0
    }
    // CHECK: _modify {
    // CHECK-NEXT: var x = 0
    // CHECK-NEXT: yield &x
    // CHECK-NEXT: }
    _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
  }

  // CHECK: public var inlinableReadNormalModify: [[INT]] {
  public var inlinableReadNormalModify: Int {
    // CHECK: @inlinable _read {
    // CHECK-NEXT: yield 0
    // CHECK-NEXT: }
    @inlinable _read {
      yield 0
    }

    // CHECK: _modify{{$}}
    // CHECK-NOT: var x = 0
    // CHECK-NOT: yield &x
    // CHECK-NOT: }
    _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
  }

  // CHECK: public var normalReadInlinableModify: [[INT]] {
  public var normalReadInlinableModify: Int {
    // CHECK: _read{{$}}
    // CHECK-NOT: yield 0
    // CHECK-NOT: }
    _read {
      yield 0
    }

    // CHECK: @inlinable _modify {
    // CHECK-NEXT: var x = 0
    // CHECK-NEXT: yield &x
    // CHECK-NEXT: }
    @inlinable _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
  }

  // CHECK: public var normalReadAndModify: [[INT]] {
  public var normalReadAndModify: Int {
    // CHECK-NEXT: _read{{$}}
    _read { yield 0 }
    // CHECK-NEXT: _modify{{$}}
    _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
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

  // CHECK: public subscript(i: [[INT]]) -> [[INT]] {
  // CHECK-NEXT:   get{{$}}
  // CHECK-NEXT:   @inlinable set[[NEWVALUE]] { print("set") }
  // CHECK-NEXT: }
  public subscript(i: Int) -> Int {
    get { return 0 }
    @inlinable set { print("set") }
  }

  // CHECK: public subscript(j: [[INT]], k: [[INT]]) -> [[INT]] {
  // CHECK-NEXT:   @inlinable get { return 0 }
  // CHECK-NEXT:   set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  public subscript(j: Int, k: Int) -> Int {
    @inlinable get { return 0 }
    set { print("set") }
  }

  // CHECK: @inlinable public subscript(l: [[INT]], m: [[INT]], n: [[INT]]) -> [[INT]] {
  // CHECK-NEXT:   get { return 0 }
  // CHECK-NEXT:   set[[NEWVALUE]] { print("set") }
  // CHECK-NEXT: }
  @inlinable
  public subscript(l: Int, m: Int, n: Int) -> Int {
    get { return 0 }
    set { print("set") }
  }

  // CHECK: public init(value: [[INT]]) {
  // CHECK-NEXT: topLevelUsableFromInline()
  // CHECK-NEXT: noAccessors = value
  // CHECK-NEXT: hasDidSet = value
  // CHECK-NEXT: }
  @inlinable public init(value: Int) {
    topLevelUsableFromInline()
    noAccessors = value
    hasDidSet = value
  }

  // CHECK: public init(){{$}}
  // CHECK-NOT: noAccessors = 0
  // CHECK-NOT: hasDidSet = 0
  public init() {
    noAccessors = 0
    hasDidSet = 0
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

// CHECK: public class HasInlinableDeinit {
public class HasInlinableDeinit {
  // CHECK: public init(){{$}}
  public init() {}

  // CHECK: [[OBJC:(@objc )?]]@inlinable deinit {
  // CHECK-NEXT: print("goodbye")
  // CHECK-NEXT: }
  @inlinable deinit {
    print("goodbye")
  }

  // CHECK-NEXT: }
}

// CHECK: public class HasStandardDeinit {
public class HasStandardDeinit {
  // CHECK: public init(){{$}}
  public init() {}

  // CHECK: [[OBJC]]deinit{{$}}
  deinit {
    print("goodbye")
  }

  // CHECK-NEXT: }
}

// CHECK: public class HasDefaultDeinit {
public class HasDefaultDeinit {
  // CHECK: public init(){{$}}
  public init() {}

  // CHECK: [[OBJC]]deinit{{$}}
  // CHECK-NEXT: }
}
