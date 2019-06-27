// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s

@propertyWrapper
public struct Wrapper<T> {
  public var value: T

  public init(initialValue: T) {
    self.value = initialValue
  }

  public init(body: () -> T) {
    self.value = body()
  }
}

var globalInt: Int { return 17 }

public struct HasWrappers {
  @Wrapper
  public var x: Int = globalInt

  @Wrapper(body: { globalInt })
  public var y: Int

  @Wrapper(body: { return globalInt })
  public var z: Int
}

func useMemberwiseInits(i: Int) {
  _ = HasWrappers(x: i)
  _ = HasWrappers(y: Wrapper(initialValue: i))
}

// CHECK: 19:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
// CHECK: 20:14 | instance-property/Swift | x | s:14swift_ide_test11HasWrappersV1xSivp | Def,RelChild | rel: 1
// CHECK: 20:23 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0

// CHECK: 22:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
// CHECK: 22:20 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0

// CHECK: 25:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
// CHECK: 25:27 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0
