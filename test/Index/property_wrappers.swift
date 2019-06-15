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
  // CHECK: [[@LINE-2]]:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
  // CHECK-NOT: [[@LINE-2]]:23 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0
  // CHECK: [[@LINE-4]]:4 | constructor/Swift | init(initialValue:) | s:14swift_ide_test7WrapperV12initialValueACyxGx_tcfc | Ref,Call,Impl | rel: 0
  // CHECK: [[@LINE-4]]:14 | instance-property/Swift | x | s:14swift_ide_test11HasWrappersV1xSivp | Def,RelChild | rel: 1
  // CHECK: [[@LINE-5]]:23 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0

  @Wrapper(body: { globalInt })
  public var y: Int
  // CHECK: [[@LINE-2]]:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
  // CHECK: [[@LINE-3]]:20 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0
  // CHECK: [[@LINE-4]]:4 | constructor/Swift | init(body:) | s:14swift_ide_test7WrapperV4bodyACyxGxyXE_tcfc | Ref,Call | rel: 0
  // CHECK: [[@LINE-4]]:14 | instance-property/Swift | y | s:14swift_ide_test11HasWrappersV1ySivp | Def,RelChild | rel: 1
  // CHECK-NOT: [[@LINE-6]]:20 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0

  @Wrapper(body: {
  // CHECK: [[@LINE-1]]:4 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
    struct Inner {
      @Wrapper
      // CHECK: [[@LINE-1]]:8 | struct/Swift | Wrapper | s:14swift_ide_test7WrapperV | Ref | rel: 0
      // CHECK: [[@LINE-2]]:8 | constructor/Swift | init(initialValue:) | s:14swift_ide_test7WrapperV12initialValueACyxGx_tcfc | Ref,Call,Impl | rel: 0
      var x: Int = globalInt
      // CHECK: [[@LINE-1]]:20 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0
    }
    return Inner().x + globalInt
    // CHECK: [[@LINE-1]]:24 | variable/Swift | globalInt | s:14swift_ide_test9globalIntSivp | Ref,Read | rel: 0
  })
  // CHECK: [[@LINE-12]]:4 | constructor/Swift | init(body:) | s:14swift_ide_test7WrapperV4bodyACyxGxyXE_tcfc | Ref,Call | rel: 0
  public var z: Int
  // CHECK: [[@LINE-1]]:14 | instance-property/Swift | z | s:14swift_ide_test11HasWrappersV1zSivp | Def,RelChild | rel: 1
}

func useMemberwiseInits(i: Int) {
  _ = HasWrappers(x: i)
  _ = HasWrappers(y: Wrapper(initialValue: i))
}
