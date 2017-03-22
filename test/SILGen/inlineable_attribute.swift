// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: sil [fragile] @_T020inlineable_attribute15fragileFunctionyyF : $@convention(thin) () -> ()
@_inlineable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV6methodyyF : $@convention(method) (MySt) -> ()
  @_inlineable public func method() {}

  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV8propertySifg : $@convention(method) (MySt) -> Int
  @_inlineable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV9subscriptS2icfg : $@convention(method) (Int, MySt) -> Int
  @_inlineable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute5MyClsCfD : $@convention(method) (@owned MyCls) -> ()
  @_inlineable deinit {}

  // Allocating entry point is [fragile]

  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute5MyClsCACyt14designatedInit_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public init(designatedInit: ()) {}

  // Note -- convenience init is intentionally not [fragile]

  // CHECK-LABEL: sil @_T020inlineable_attribute5MyClsCACyt15convenienceInit_tcfC : $@convention(method) (@thick MyCls.Type) -> @owned MyCls
  public convenience init(convenienceInit: ()) {
    self.init(designatedInit: ())
  }
}

// Make sure enum case constructors for public and versioned enums are
// [fragile].
@_versioned enum MyEnum {
  case c(MySt)
}

// CHECK-LABEL: sil shared [transparent] [fragile] [thunk] @_T020inlineable_attribute6MyEnumO1cAcA0C2StVcACmFTc : $@convention(thin) (@thin MyEnum.Type) -> @owned @callee_owned (MySt) -> MyEnum

@_inlineable public func referencesMyEnum() {
  _ = MyEnum.c
}

// CHECK-LABEL: sil [transparent] @_T020inlineable_attribute15HasInitializersV1xSivfi : $@convention(thin) () -> Int

public struct HasInitializers {
  public let x = 1234

  @_inlineable public init() {}
}
