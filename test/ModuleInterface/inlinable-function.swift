// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix CHECK < %t/Test.swiftinterface
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test
// RUN: %FileCheck %s --check-prefix FROMMODULE --check-prefix CHECK < %t/TestFromModule.swiftinterface

// FIXME: These shouldn't be different, or we'll get different output from
// WMO and non-WMO builds.
// CHECK-LABEL: public struct Foo : Swift.Hashable {
public struct Foo: Hashable {
  // CHECK: public var inlinableGetPublicSet: Swift.Int {
  public var inlinableGetPublicSet: Int {
  // FROMSOURCE: @inlinable get {
  // FROMMODULE: @inlinable get{{$}}
  // FROMSOURCE-NEXT: return 3
  // FROMSOURCE-NEXT: }
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

  // CHECK: public var noAccessors: Swift.Int{{$}}
  public var noAccessors: Int

  // CHECK: public var hasDidSet: Swift.Int {
  public var hasDidSet: Int {
    // CHECK-NEXT: @_transparent get{{$}}
    // CHECK-NEXT: set{{(\(value\))?}}{{$}}
    // CHECK-NOT: didSet
    didSet {
      print("b set to \(hasDidSet)")
    }
    // CHECK-NEXT: {{^}}  }
  }

  // CHECK: @_transparent public var transparent: Swift.Int {
  // FROMMODULE-NEXT: get{{$}}
  // FROMSOURCE-NEXT: get {
  // FROMSOURCE-NEXT:   return 34
  // FROMSOURCE-NEXT: }
  // CHECK-NEXT: }
  @_transparent
  public var transparent: Int {
    return 34
  }

  // CHECK: public var transparentSet: Swift.Int {
  public var transparentSet: Int {
    // CHECK-NEXT: get{{$}}
    get {
      return 34
    }
    // FROMMODULE-NEXT: @_transparent set[[NEWVALUE]]{{$}}
    // FROMSOURCE-NEXT: @_transparent set[[NEWVALUE]] {
    // FROMSOURCE-NOT:   #if false
    // FROMSOURCE-NOT:   print("I should not appear")
    // FROMSOURCE-NOT:   #else
    // FROMSOURCE-NOT:   #if false
    // FROMSOURCE-NOT:   print("I also should not")
    // FROMSOURCE-NOT:   #else
    // FROMSOURCE:       print("I am set to \(newValue)")
    // FROMSOURCE-NOT:   #endif
    // FROMSOURCE-NOT:   #endif
    // FROMSOURCE-NEXT: }
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

  // CHECK: @inlinable public var inlinableProperty: Swift.Int {
  @inlinable
  public var inlinableProperty: Int {
    // FROMMODULE:      get{{$}}
    // FROMSOURCE:      get {
    // FROMSOURCE-NEXT:   return 32
    // FROMSOURCE-NEXT: }
    get {
      return 32
    }

    // FROMMODULE: set[[NEWVALUE]]{{$}}
    // FROMSOURCE: set[[NEWVALUE]] {
    // FROMSOURCE-NOT: #if true
    // FROMSOURCE:     print("I am set to \(newValue)")
    // FROMSOURCE-NOT: #else
    // FROMSOURCE-NOT: print("I should not appear")
    // FROMSOURCE-NOT  #endif
    // FROMSOURCE: }
    set {
      #if true
      print("I am set to \(newValue)")
      #else
      print("I should not appear")
      #endif
    }
    // CHECK-NEXT: }
  }

  // CHECK: @inlinable public var inlinableReadAndModify: Swift.Int {
  @inlinable
  public var inlinableReadAndModify: Int {
    // FROMMODULE:      _read{{$}}
    // FROMSOURCE:      _read {
    // FROMSOURCE-NEXT:   yield 0
    // FROMSOURCE-NEXT: }
    _read {
      yield 0
    }
    // FROMMODULE:      _modify{{$}}
    // FROMSOURCE:      _modify {
    // FROMSOURCE-NEXT:   var x = 0
    // FROMSOURCE-NEXT:   yield &x
    // FROMSOURCE-NEXT: }
    _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
  }

  // CHECK: public var inlinableReadNormalModify: Swift.Int {
  public var inlinableReadNormalModify: Int {
    // FROMMODULE: @inlinable _read{{$}}
    // FROMSOURCE: @inlinable _read {
    // FROMSOURCE-NEXT: yield 0
    // FROMSOURCE-NEXT: }
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

  // CHECK: public var normalReadInlinableModify: Swift.Int {
  public var normalReadInlinableModify: Int {
    // CHECK: _read{{$}}
    // CHECK-NOT: yield 0
    // CHECK-NOT: }
    _read {
      yield 0
    }

    // FROMMODULE: @inlinable _modify{{$}}
    // FROMSOURCE: @inlinable _modify {
    // FROMSOURCE-NEXT: var x = 0
    // FROMSOURCE-NEXT: yield &x
    // FROMSOURCE-NEXT: }
    @inlinable _modify {
      var x = 0
      yield &x
    }
    // CHECK-NEXT: }
  }

  // CHECK: public var normalReadAndModify: Swift.Int {
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

  // FROMMODULE: @inlinable public func inlinableMethod(){{$}}
  // FROMSOURCE: @inlinable public func inlinableMethod() {
  // FROMSOURCE-NOT: #if NO
  // FROMSOURCE-NOT: print("Hello, world!")
  // FROMSOURCE-NOT: #endif
  // FROMSOURCE:     print("Goodbye, world!")
  // FROMSOURCE-NEXT: }
  @inlinable
  public func inlinableMethod() {
    #if NO
    print("Hello, world!")
    #endif
    print("Goodbye, world!")
  }


  // FROMMODULE: @_transparent [[ATTRS:(mutating public|public mutating)]] func transparentMethod(){{$}}
  // FROMSOURCE: @_transparent [[ATTRS:(mutating public|public mutating)]] func transparentMethod() {
  // FROMSOURCE-NEXT:   inlinableProperty = 4
  // FROMSOURCE-NEXT: }
  @_transparent
  mutating public func transparentMethod() {
    inlinableProperty = 4
  }

  // CHECK: public func nonInlinableMethod(){{$}}
  // CHECK-NOT: print("Not inlinable")
  public func nonInlinableMethod() {
    print("Not inlinable")
  }

  // CHECK: public subscript(i: Swift.Int) -> Swift.Int {
  // CHECK-NEXT:   get{{$}}
  // FROMSOURCE-NEXT:   @inlinable set[[NEWVALUE]] { print("set") }
  // FROMMODULE-NEXT:   @inlinable set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  public subscript(i: Int) -> Int {
    get { return 0 }
    @inlinable set { print("set") }
  }

  // CHECK: public subscript(j: Swift.Int, k: Swift.Int) -> Swift.Int {
  // FROMMODULE-NEXT:   @inlinable get{{$}}
  // FROMSOURCE-NEXT:   @inlinable get { return 0 }
  // CHECK-NEXT:   set[[NEWVALUE]]{{$}}
  // CHECK-NEXT: }
  public subscript(j: Int, k: Int) -> Int {
    @inlinable get { return 0 }
    set { print("set") }
  }

  // CHECK: @inlinable public subscript(l: Swift.Int, m: Swift.Int, n: Swift.Int) -> Swift.Int {
  // FROMMODULE-NEXT:   get{{$}}
  // FROMSOURCE-NEXT:   get { return 0 }
  // FROMMODULE-NEXT:   set[[NEWVALUE]]{{$}}
  // FROMSOURCE-NEXT:   set[[NEWVALUE]] { print("set") }
  // CHECK-NEXT: }
  @inlinable
  public subscript(l: Int, m: Int, n: Int) -> Int {
    get { return 0 }
    set { print("set") }
  }

  // FROMMODULE: @inlinable public init(value: Swift.Int){{$}}
  // FROMSOURCE: @inlinable public init(value: Swift.Int) {
  // FROMSOURCE-NEXT: topLevelUsableFromInline()
  // FROMSOURCE-NEXT: noAccessors = value
  // FROMSOURCE-NEXT: hasDidSet = value
  // FROMSOURCE-NEXT: }
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

// FROMMODULE: @inlinable public func topLevelInlinable(){{$}}
// FROMSOURCE: @inlinable public func topLevelInlinable() {
// FROMSOURCE-NEXT:  topLevelUsableFromInline()
// FROMSOURCE-NEXT: }
@inlinable public func topLevelInlinable() {
  topLevelUsableFromInline()
}

// CHECK: public class HasInlinableDeinit {
public class HasInlinableDeinit {
  // CHECK: public init(){{$}}
  public init() {}

  // FROMMODULE: [[OBJC:(@objc )?]]@inlinable deinit{{$}}
  // FROMSOURCE: [[OBJC:(@objc )?]]@inlinable deinit {
  // FROMSOURCE-NEXT: print("goodbye")
  // FROMSOURCE-NEXT: }
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
