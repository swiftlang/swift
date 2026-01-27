// RUN: %target-swift-emit-silgen                           \
// RUN:     %s                                              \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN: | %FileCheck %s --check-prefixes=CHECK

// TODO: CoroutineAccessors: Change to %target-swift-x.y-abi-triple
// RUN: %target-swift-emit-silgen                           \
// RUN:     %s                                              \
// RUN:     -enable-library-evolution                       \
// RUN:     -module-name Library                            \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -library-level=api                              \
// RUN:     -target %target-future-triple                   \
// RUN: | %FileCheck %s --check-prefixes=CHECK

// REQUIRES: swift_feature_CoroutineAccessors

// UNSUPPORTED: OS=windows-msvc

// When CoroutineAccessors are enabled we want to generate

// For explicit/implicit _read/_modify in source:
//  For old clients (that have not been "recompiled"/an old deployment target:
//  * the old coroutine entry pointes (yield_once): _read, _modify
//  For new clients that have been "recompiled" with an deployment target
//    >= CoroutineAccessors introduction:
//  * the new coroutine entry points (yield_once_2):  yielding borrow/mutate

// Ditto For explicit/implicit yielding borrow/yielding mutate in source:
//  For old clients (that have not been "recompiled"/an old deployment target:
//  (this is because _read/_modify could have been updated to the new accessors.
//  * the old coroutine entry pointes (yield_once): _read, _modify
//  For new clients that have been "recompiled" with an deployment target
//    >= CoroutineAccessors introduction:
//  * the new coroutine entry points (yield_once_2):  yielding borrow/mutate

/// Tests with _read/_modify/set

public struct ResilientStruct {
  var _s : Int = 0

  public var s : Int {
    _read {
      yield _s
    }

    _modify {
      yield &_s
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientStructV{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientStructV{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientStructV{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientStructV{{.*}}vx : $@yield_once_2 @conv

// A `set` implies an implicit guaranteed `_modify` accessor.
public struct ResilientStructWithImplicitModify {
  var _s : Int = 0

  public var s : Int {
    _read {
      yield _s
    }

    set {
      _s = newValue
    }
  }
}
// _read
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyV{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyV{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyV{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyV{{.*}}vx : $@yield_once_2 @conv

public enum ResilientEnum {
  case a(Int)
  case b(Int)

  public var s : Int {
    _read {
      switch self {
        case .a(let i):
          yield i
        case .b(let i):
          yield i
      }
    }

    _modify {
      switch self {
        case .a(let i):
          var tmp = i
          yield &tmp
          self = .a(tmp)
        case .b(let i):
          var tmp = i
          yield &tmp
          self = .b(tmp)
      }
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientEnumO{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientEnumO{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientEnumO{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientEnumO{{.*}}vx : $@yield_once_2 @conv

public class ResilientClass {
  var _s : Int = 0

  public var s : Int {
    _read {
      yield _s
    }

    _modify {
      yield &_s
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientClassC{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientClassC{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientClassC{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientClassC{{.*}}vx : $@yield_once_2 @conv



/// Tests with yielding borrow/yielding mutate/set

public struct ResilientStructY {
  var _s : Int = 0

  public var s : Int {
    yielding borrow {
      yield _s
    }

    yielding mutate {
      yield &_s
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientStructYV{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientStructYV{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientStructYV{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientStructYV{{.*}}vx : $@yield_once_2 @conv

// A `set` implies an implicit guaranteed `_modify` accessor.
public struct ResilientStructWithImplicitModifyY {
  var _s : Int = 0

  public var s : Int {
    yielding borrow {
      yield _s
    }

    set {
      _s = newValue
    }
  }
}
// _read
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyYV{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyYV{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyYV{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientStructWithImplicitModifyYV{{.*}}vx : $@yield_once_2 @conv

public enum ResilientEnumY {
  case a(Int)
  case b(Int)

  public var s : Int {
    yielding borrow {
      switch self {
        case .a(let i):
          yield i
        case .b(let i):
          yield i
      }
    }

    yielding mutate {
      switch self {
        case .a(let i):
          var tmp = i
          yield &tmp
          self = .a(tmp)
        case .b(let i):
          var tmp = i
          yield &tmp
          self = .b(tmp)
      }
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientEnumYO{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientEnumYO{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientEnumYO{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientEnumYO{{.*}}vx : $@yield_once_2 @conv

public class ResilientClassY {
  var _s : Int = 0

  public var s : Int {
    yielding borrow {
      yield _s
    }

    yielding mutate {
      yield &_s
    }
  }
}

// _read
// CHECK-DAG:  sil {{.*}}ResilientClassYC{{.*}}vr : $@yield_once @conv
// _modify
// CHECK-DAG:  sil {{.*}}ResilientClassYC{{.*}}vM : $@yield_once @conv
// yielding borrow
// CHECK-DAG:  sil {{.*}}ResilientClassYC{{.*}}vy : $@yield_once_2 @conv
// yielding mutate
// CHECK-DAG:  sil {{.*}}ResilientClassYC{{.*}}vx : $@yield_once_2 @conv
