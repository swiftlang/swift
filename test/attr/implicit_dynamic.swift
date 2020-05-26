// RUN: %target-swift-frontend -swift-version 5 -enable-implicit-dynamic  -I %t -emit-silgen %s | %FileCheck %s

// Make sure that these functions are not implicitly marked dynamic.

public struct NotImplicitDynamic {
  @inlinable
  public var x : Int {
// CHECK: sil [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV1xSivg
// CHECK: sil [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV1xSivs
    get {
      return 1
    }
    set {
    }
  }

  @inlinable
  public var y : Int {
// CHECK: sil [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV1ySivg
    return 1
  }

  public var z : Int {
// CHECK: sil [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV1zSivg
// CHECK: sil [ossa] @$s16implicit_dynamic18NotImplicitDynamicV1zSivs
    @inlinable
    get {
      return 1
    }
    set {
    }
  }

  @_transparent
  public var x2 : Int {
// CHECK: sil [transparent] [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV2x2Sivg
// CHECK: sil [transparent] [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicV2x2Sivs
    get {
      return 1
    }
    set {
    }
  }

  public subscript() -> Int {
// CHECK: sil [transparent] [serialized] [ossa] @$s16implicit_dynamic18NotImplicitDynamicVSiycig
    @_transparent
    get{
      return 1
    }
  }
}

// CHECK: sil [ossa] @foobar
@_silgen_name("foobar")
public func noImplicitDynamicFunc() {
}
