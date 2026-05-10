// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// Makes sure the modify coroutine is not @_transparent, since it references
// private properties.

public class Store {
  @Published public var state: Any
  init() {}
}

@propertyWrapper public struct Published<Value> {
  public init(wrappedValue: Value) {}
  public var wrappedValue: Value {
    get {}
    set {}
  }
  public static subscript<EnclosingSelf>(
        _enclosingInstance object: EnclosingSelf,
        wrapped wrappedKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Value>,
        storage storageKeyPath: ReferenceWritableKeyPath<EnclosingSelf, Published<Value>>)
      -> Value where EnclosingSelf : AnyObject {
    get {}
    set {}
  }
  public struct Publisher {}
  public var projectedValue: Publisher {
    mutating get {}
  }
}

// CHECK-LABEL: sil [ossa] @$s33property_wrapper_coroutine_public5StoreC5stateypvM : $@yield_once @convention(method) (@guaranteed Store) -> @yields @inout Any {
// CHECK: keypath $ReferenceWritableKeyPath<Store, Any>, (root $Store; settable_property $Any,  id #Store.state!getter : (Store) -> () -> Any, getter @$s33property_wrapper_coroutine_public5StoreC5stateypvpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed Store) -> @out Any, setter @$s33property_wrapper_coroutine_public5StoreC5stateypvpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Any, @in_guaranteed Store) -> ())
// CHECK: keypath $ReferenceWritableKeyPath<Store, Published<Any>>, (root $Store; stored_property #Store._state : $Published<Any>)
