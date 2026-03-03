// RUN:%target-swift-frontend -emit-irgen %s -verify  -enable-experimental-feature BorrowAndMutateAccessors | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: OS=macosx

public class Klass {
  var id: Int = 0
}

func getKlass() -> Klass {
  return Klass()
}

@inline(never)
func use(_ k: Klass) {
  print(k.id)
}

@inline(never)
func use(_ tuple: (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass)) {
  print(tuple.4.id)
}

public struct NC : ~Copyable {
  var id: Int = 0
}

@inline(never)
func use(_ t: borrowing NC) {
  print(t.id)
}

public struct S {
  var _k: Klass

  var k: Klass {
    borrow {
      return _k
    }
  }
}

public struct Wrapper {
  var _k: Klass
  var _s: S

  var s: S {
    borrow {
      return _s
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
  }

  var nested1: Klass {
    borrow {
      return _s.k
    }
  }

  var nested2: Klass {
    borrow {
      return k
    }
  }

  subscript(index: Int) -> Klass {
    borrow {
      return _k
    }
  }

  var nested_subscript: Klass {
    borrow {
      return self[0]
    }
  }
}

public struct SimpleWrapper<T> {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
  }
}

public struct GenWrapper<T> {
  var _prop: T
  var _s: SimpleWrapper<T>
  var _k: Klass

  public var prop: T {
    borrow {
      return _prop
    }
  }

  var nested1: T {
    borrow {
      return _s.prop
    }
  }

  var nested2: T {
    borrow {
      return prop
    }
  }

  var k: Klass {
    borrow {
      return _k
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
  }

  var nested_subscript: T {
    borrow {
      return self[0]
    }
  }
}

public struct SimpleNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T

  var prop: T {
    borrow {
      return _prop
    }
  }
}

public struct GenNCWrapper<T : ~Copyable> : ~Copyable {
  var _prop: T
  var _s: SimpleNCWrapper<T>

  public var prop: T {
    borrow {
      return _prop
    }
  }

  var nested1: T {
    borrow {
      return _s.prop
    }
  }

  var nested2: T {
    borrow {
      return prop
    }
  }

  subscript(index: Int) -> T {
    borrow {
      return _prop
    }
  }

  var nested_subscript: T {
    borrow {
      return self[0]
    }
  }
}

public struct NCS: ~Copyable {
  var _nc: NC

  var nc: NC {
    borrow {
      return _nc
    }
  }
}

public struct NCWrapper: ~Copyable {
  var _nc: NC
  var _s: NCS

  var nc: NC {
    borrow {
      return _nc
    }
  }
  var nested1: NC {
    borrow {
      return _s.nc
    }
  }

  var nested2: NC {
    borrow {
      return nc
    }
  }

  subscript(index: Int) -> NC {
    borrow {
      return _nc
    }
  }

  var nested_subscript: NC {
    borrow {
      return self[0]
    }
  }
}

public struct LargeStruct {
  var _k: Klass
  var _t: (Int, Int, Int, Int, Int, Int, Int, Int)
  var _tuple: (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass)

  var borrowKlass: Klass {
    borrow {
      return _k
    }
  }
  var borrowTuple: (Klass, Klass, Klass, Klass, Klass, Klass, Klass, Klass) {
    borrow {
      return _tuple
    }
  }
}

func test() {
  let w1 = Wrapper(_k: Klass(), _s: S(_k: Klass()))
  use(w1.k)
  use(w1.s.k)
  use(w1.nested1)
  use(w1.nested2)

  let w2 = GenWrapper(_prop: Klass(), _s: SimpleWrapper(_prop: Klass()), _k: Klass())
  use(w2.prop)
  use(w2.nested1)
  use(w2.nested2)

  let w3 = GenNCWrapper(_prop: NC(), _s: SimpleNCWrapper(_prop: NC()))
  use(w3.prop)
  use(w3.nested1)
  use(w3.nested2)

  let l = LargeStruct(_k: Klass(), _t: (1,2,3,4,5,6,7,8), _tuple: (Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass(), Klass()))
  use(l.borrowKlass)
  use(l.borrowTuple)
}

// IRGen explodes the struct parameter and returns the specified field
// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor7WrapperV1kAA5KlassCvb"(ptr [[REG0:%.*]], ptr [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG0]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor7WrapperV7nested1AA5KlassCvb"(ptr [[REG0:%.*]], ptr [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor1SV1kAA5KlassCvb"(ptr [[REG1]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor7WrapperV7nested2AA5KlassCvb"(ptr [[REG0:%.*]], ptr [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor7WrapperV1kAA5KlassCvb"(ptr [[REG0]], ptr [[REG1]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor7WrapperVyAA5KlassCSicib"(i64 [[REG0:%.*]], ptr [[REG1:%.*]], ptr [[REG2:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG1]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor7WrapperV16nested_subscriptAA5KlassCvb"(ptr [[REG0:%.*]], ptr [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor7WrapperVyAA5KlassCSicib"(i64 0, ptr [[REG0]], ptr [[REG1]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define swiftcc ptr @"$s15borrow_accessor10GenWrapperV4propxvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG0]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor10GenWrapperV7nested1xvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = getelementptr inbounds i32, ptr %"GenWrapper<T>", i64 7
// CHECK:   [[REG3:%.*]] = load i32, ptr [[REG2]], align 8
// CHECK:   [[REG4:%.*]] = getelementptr inbounds i8, ptr [[REG0]], i32 [[REG3]]
// CHECK:   [[REG5:%.*]] = call swiftcc %swift.metadata_response @"$s15borrow_accessor13SimpleWrapperVMa"(i64 0, ptr %T)
// CHECK:   [[REG6:%.*]] = extractvalue %swift.metadata_response [[REG5]], 0
// CHECK:   [[REG7:%.*]] = call swiftcc ptr @"$s15borrow_accessor13SimpleWrapperV4propxvb"(ptr [[REG6]], ptr noalias swiftself [[REG4]])
// CHECK:   ret ptr [[REG7]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor10GenWrapperV7nested2xvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor10GenWrapperV4propxvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor10GenWrapperV1kAA5KlassCvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = getelementptr inbounds i32, ptr %"GenWrapper<T>", i64 8
// CHECK:   [[REG3:%.*]] = load i32, ptr [[REG2]], align 8
// CHECK:   [[REG4:%.*]] = getelementptr inbounds i8, ptr [[REG0]], i32 [[REG3]]
// CHECK:   [[REG5:%.*]] = load ptr, ptr [[REG4]], align 8
// CHECK:   ret ptr [[REG5]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor10GenWrapperVyxSicib"(i64 [[REG0:%.*]], ptr %"GenWrapper<T>", ptr noalias swiftself [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG1]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor10GenWrapperV16nested_subscriptxvb"(ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor10GenWrapperVyxSicib"(i64 0, ptr %"GenWrapper<T>", ptr noalias swiftself [[REG0]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvb"(ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG0]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlE7nested1xvb"(ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = getelementptr inbounds i32, ptr %"GenNCWrapper<T>", i64 7
// CHECK:   [[REG3:%.*]] = load i32, ptr [[REG2]], align 8
// CHECK:   [[REG4:%.*]] = getelementptr inbounds i8, ptr [[REG0]], i32 [[REG3]]
// CHECK:   [[REG5:%.*]] = call swiftcc %swift.metadata_response @"$s15borrow_accessor15SimpleNCWrapperVMa"(i64 0, ptr %T)
// CHECK:   [[REG6:%.*]] = extractvalue %swift.metadata_response [[REG5]], 0
// CHECK:   [[REG7:%.*]] = call swiftcc ptr @"$s15borrow_accessor15SimpleNCWrapperVAARi_zrlE4propxvb"(ptr [[REG6]], ptr noalias swiftself [[REG4]])
// CHECK:   ret ptr [[REG7]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlE7nested2xvb"(ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlE4propxvb"(ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSicib"(i64 [[REG0:%.*]], ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret ptr [[REG1]]
// CHECK: }

// CHECK: define hidden swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlE16nested_subscriptxvb"(ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc ptr @"$s15borrow_accessor12GenNCWrapperVAARi_zrlEyxSicib"(i64 0, ptr %"GenNCWrapper<T>", ptr noalias swiftself [[REG0]])
// CHECK:   ret ptr [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc i64 @"$s15borrow_accessor9NCWrapperV2ncAA2NCVvb"(i64 [[REG0:%.*]], i64 [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret i64 [[REG0]]
// CHECK: }

// CHECK: define hidden swiftcc i64 @"$s15borrow_accessor9NCWrapperV7nested1AA2NCVvb"(i64 [[REG0:%.*]], i64 [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc i64 @"$s15borrow_accessor3NCSV2ncAA2NCVvb"(i64 [[REG1]])
// CHECK:   ret i64 [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc i64 @"$s15borrow_accessor9NCWrapperV7nested2AA2NCVvb"(i64 [[REG0:%.*]], i64 [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc i64 @"$s15borrow_accessor9NCWrapperV2ncAA2NCVvb"(i64 [[REG0]], i64 [[REG1]])
// CHECK:   ret i64 [[REG2]]
// CHECK: }

// CHECK: define hidden swiftcc i64 @"$s15borrow_accessor9NCWrapperVyAA2NCVSicib"(i64 [[REG0]], i64 [[REG1]], i64 [[REG2]]) {{.*}} {
// CHECK: entry:
// CHECK:   ret i64 [[REG1]]
// CHECK: }

// CHECK: define hidden swiftcc i64 @"$s15borrow_accessor9NCWrapperV16nested_subscriptAA2NCVvb"(i64 [[REG0:%.*]], i64 [[REG1:%.*]]) {{.*}} {
// CHECK: entry:
// CHECK:   [[REG2:%.*]] = call swiftcc i64 @"$s15borrow_accessor9NCWrapperVyAA2NCVSicib"(i64 0, i64 [[REG0]], i64 [[REG1]])
// CHECK:   ret i64 [[REG2]]
// CHECK: }
