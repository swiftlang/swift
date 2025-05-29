// RUN: %target-swift-frontend -swift-version 5 -typecheck -dump-ast %s | %FileCheck  %s

struct Strukt {
  // CHECK: (struct_decl {{.*}} "Strukt"
  // CHECK: (var_decl {{.*}} "dynamicStorageOnlyVar" interface_type="Int" access=internal dynamic readImpl=stored writeImpl=stored readWriteImpl=stored
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="dynamicStorageOnlyVar"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="dynamicStorageOnlyVar"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="dynamicStorageOnlyVar"
  dynamic var dynamicStorageOnlyVar : Int = 0

  // CHECK: (var_decl {{.*}} "computedVar" interface_type="Int" access=internal dynamic readImpl=getter immutable
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVar"
  dynamic var computedVar : Int {
    return 0
  }

  // CHECK: (var_decl {{.*}} "computedVar2" interface_type="Int" access=internal dynamic readImpl=getter immutable
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVar2"
  dynamic var computedVar2 : Int {
    get {
      return 0
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarGetterSetter" interface_type="Int" access=internal dynamic readImpl=getter writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVarGetterSetter"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="computedVarGetterSetter"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="computedVarGetterSetter"
  dynamic var computedVarGetterSetter : Int {
    get {
      return 0
    }
    set {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarGetterModify" interface_type="Int" access=internal dynamic readImpl=getter writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVarGetterModify"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="computedVarGetterModify"
  // CHECK: (accessor_decl {{.*}} access=internal set for="computedVarGetterModify"
  dynamic var computedVarGetterModify : Int {
    get {
      return 0
    }
    _modify {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarReadSet" interface_type="Int" access=internal dynamic readImpl=read_coroutine writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal get for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="computedVarReadSet"
  dynamic var computedVarReadSet : Int {
    _read {
    }
    set {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarReadModify" interface_type="Int" access=internal dynamic readImpl=read_coroutine writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal get for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal set for="computedVarReadModify"
  dynamic var computedVarReadModify : Int {
    _read {
    }
    _modify {
    }
  }

  // CHECK: (var_decl {{.*}} "storedWithObserver" interface_type="Int" access=internal dynamic readImpl=stored writeImpl=stored_with_observers readWriteImpl=stored_with_didset
  // CHECK: (accessor_decl {{.*}}access=private dynamic didSet for="storedWithObserver"
  // CHECK: (accessor_decl {{.*}}access=internal dynamic get for="storedWithObserver"
  // CHECK: (accessor_decl {{.*}}access=internal set for="storedWithObserver"
  // CHECK: (accessor_decl {{.*}}access=internal _modify for="storedWithObserver"
  dynamic var storedWithObserver : Int {
    didSet {
    }
  }

  // CHECK: (func_decl {{.*}} access=internal dynamic
  dynamic func aMethod(arg: Int) -> Int {
    return arg
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=getter writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="subscript(_:)"
  dynamic subscript(_ index: Int) -> Int {
    get {
      return 1
    }
    set {
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=getter writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  dynamic subscript(_ index: Float) -> Int {
    get {
      return 1
    }
    _modify {
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=read_coroutine writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  dynamic subscript(_ index: Double) -> Int {
    _read {
    }
    _modify {
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=read_coroutine writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="subscript(_:)"
  dynamic subscript(_ index: Strukt) -> Int {
    _read {
    }
    set {
    }
  }
}

class Klass {
  // CHECK: (class_decl {{.*}} "Klass"
  // CHECK: (var_decl {{.*}} "dynamicStorageOnlyVar" interface_type="Int" access=internal dynamic readImpl=stored writeImpl=stored readWriteImpl=stored
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="dynamicStorageOnlyVar"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="dynamicStorageOnlyVar"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="dynamicStorageOnlyVar"
  dynamic var dynamicStorageOnlyVar : Int = 0

  // CHECK: (var_decl {{.*}} "computedVar" interface_type="Int" access=internal dynamic readImpl=getter immutable
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVar"
  dynamic var computedVar : Int {
    return 0
  }

  // CHECK: (var_decl {{.*}} "computedVar2" interface_type="Int" access=internal dynamic readImpl=getter immutable
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVar2"
  dynamic var computedVar2 : Int {
    get {
      return 0
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarGetterSetter" interface_type="Int" access=internal dynamic readImpl=getter writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVarGetterSetter"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="computedVarGetterSetter"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="computedVarGetterSetter"
  dynamic var computedVarGetterSetter : Int {
    get {
      return 0
    }
    set {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarGetterModify" interface_type="Int" access=internal dynamic readImpl=getter writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="computedVarGetterModify"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="computedVarGetterModify"
  // CHECK: (accessor_decl {{.*}} access=internal set for="computedVarGetterModify"
  dynamic var computedVarGetterModify : Int {
    get {
      return 0
    }
    _modify {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarReadSet" interface_type="Int" access=internal dynamic readImpl=read_coroutine writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal get for="computedVarReadSet"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="computedVarReadSet"
  dynamic var computedVarReadSet : Int {
    _read {
    }
    set {
    }
  }

  // CHECK: (var_decl {{.*}} "computedVarReadModify" interface_type="Int" access=internal dynamic readImpl=read_coroutine writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal get for="computedVarReadModify"
  // CHECK: (accessor_decl {{.*}} access=internal set for="computedVarReadModify"
  dynamic var computedVarReadModify : Int {
    _read {
    }
    _modify {
    }
  }
  // CHECK: (func_decl {{.*}} "aMethod(arg:)" {{.*}} access=internal dynamic
  dynamic func aMethod(arg: Int) -> Int {
    return arg
  }

  // CHECK-NOT: (func_decl {{.*}} "anotherMethod()" {{.*}} access=internal{{.*}} dynamic
  func anotherMethod() -> Int {
    return 3
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=addressor writeImpl=mutable_addressor readWriteImpl=mutable_addressor
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeMutableAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  dynamic subscript(_ index: Int) -> Int {
    unsafeAddress {
      fatalError()
    }
    unsafeMutableAddress {
      fatalError()
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=getter writeImpl=mutable_addressor readWriteImpl=mutable_addressor
  // CHECK: (accessor_decl {{.*}} access=internal dynamic get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeMutableAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="subscript(_:)"
  dynamic subscript(_ index: Float) -> Int {
    get {
      return 1
    }
    unsafeMutableAddress {
      fatalError()
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=read_coroutine writeImpl=mutable_addressor readWriteImpl=mutable_addressor
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _read for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeMutableAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="subscript(_:)"
  dynamic subscript(_ index: Double) -> Int {
    _read {
    }
    unsafeMutableAddress {
      fatalError()
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=addressor writeImpl=setter readWriteImpl=materialize_to_temporary
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic set for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal _modify for="subscript(_:)"
  dynamic subscript(_ index: Int8) -> Int {
    unsafeAddress {
      fatalError()
    }
    set {
    }
  }

  // CHECK: (subscript_decl {{.*}} "subscript(_:)" {{.*}} access=internal dynamic readImpl=addressor writeImpl=modify_coroutine readWriteImpl=modify_coroutine
  // CHECK: (accessor_decl {{.*}} access=internal dynamic unsafeAddress for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal dynamic _modify for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal get for="subscript(_:)"
  // CHECK: (accessor_decl {{.*}} access=internal set for="subscript(_:)"
  dynamic subscript(_ index: Int16) -> Int {
    unsafeAddress {
      fatalError()
    }
    _modify {
    }
  }
}

class SubKlass : Klass {

  // CHECK: (class_decl {{.*}} "SubKlass"
  // CHECK: (func_decl {{.*}} "aMethod(arg:)" interface_type="(SubKlass) -> (Int) -> Int" access=internal {{.*}} dynamic
  override dynamic func aMethod(arg: Int) -> Int {
   return 23
  }

  // CHECK: (func_decl {{.*}} "anotherMethod()" interface_type="(SubKlass) -> () -> Int" access=internal {{.*}} dynamic
  override dynamic func anotherMethod() -> Int {
   return 23
  }
}
