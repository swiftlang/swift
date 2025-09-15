// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck -check-prefix=CHECK %s

@propertyWrapper
public struct Wrapper<T> {
  // CHECK: [[@LINE-1]]:15 | struct/Swift | Wrapper | [[Wrapper_USR:.*]] | Def | rel: 0
  public var wrappedValue: T
  // CHECK: [[@LINE-1]]:14 | instance-property/Swift | wrappedValue | [[wrappedValue_USR:.*]] | Def,RelChild | rel: 1

  public init(initialValue: T) {
  // CHECK: [[@LINE-1]]:10 | constructor/Swift | init(initialValue:) | [[WrapperInit_USR:.*]] | Def,RelChild | rel: 1
    self.wrappedValue = initialValue
  }

  public init(body: () -> T) {
  // CHECK: [[@LINE-1]]:10 | constructor/Swift | init(body:) | [[WrapperBodyInit_USR:.*]] | Def,RelChild | rel: 1
    self.wrappedValue = body()
  }

  public var projectedValue: Projection<T> {
    get { Projection(item: wrappedValue) }
  }
}

public struct Projection<T> {
    var item: T
}

var globalInt: Int { return 17 }
// CHECK: [[@LINE-1]]:5 | variable/Swift | globalInt | [[globalInt_USR:.*]] | Def | rel: 0

public struct HasWrappers {
  @Wrapper
  // CHECK: [[@LINE-1]]:4 | struct/Swift | Wrapper | [[Wrapper_USR]] | Ref,RelCont | rel: 1
  public var x: Int = globalInt
  // CHECK-NOT: [[@LINE-1]]:23 | variable/Swift | globalInt
  // CHECK: [[@LINE-4]]:4 | constructor/Swift | init(initialValue:) | [[WrapperInit_USR]] | Ref,Call,Impl,RelCont | rel: 1
  // CHECK: [[@LINE-3]]:14 | instance-property/Swift | x | [[x_USR:.*]] | Def,RelChild | rel: 1
  // CHECK: [[@LINE-4]]:23 | variable/Swift | globalInt | [[globalInt_USR]] | Ref,Read,RelCont | rel: 1

  @Wrapper(body: { globalInt })
  // CHECK: [[@LINE-1]]:4 | struct/Swift | Wrapper | [[Wrapper_USR]] | Ref,RelCont | rel: 1
  // CHECK: [[@LINE-2]]:20 | variable/Swift | globalInt | [[globalInt_USR]] | Ref,Read,RelCont | rel: 1
  // CHECK: [[@LINE-3]]:4 | constructor/Swift | init(body:) | [[WrapperBodyInit_USR]] | Ref,Call,RelCont | rel: 1
  public var y: Int
  // CHECK: [[@LINE-1]]:14 | instance-property/Swift | y | [[y_USR:.*]] | Def,RelChild | rel: 1
  // CHECK-NOT: [[@LINE-6]]:20 | variable/Swift | globalInt

  @Wrapper(body: {
  // CHECK:      [[@LINE-1]]:4 | struct/Swift | Wrapper | [[Wrapper_USR]] | Ref,RelCont | rel: 1
  // CHECK-NEXT: RelCont | instance-property/Swift | z
    struct Inner {
      @Wrapper
      // CHECK:      [[@LINE-1]]:8 | struct/Swift | Wrapper | [[Wrapper_USR]] | Ref,RelCont | rel: 1
      // CHECK-NEXT: RelCont | instance-property/Swift | z
      // CHECK:      [[@LINE-3]]:8 | constructor/Swift | init(initialValue:) | [[WrapperInit_USR]] | Ref,Call,Impl,RelCont | rel: 1
      // CHECK-NEXT: RelCont | instance-property/Swift | z
      var x: Int = globalInt
      // CHECK: [[@LINE-1]]:20 | variable/Swift | globalInt | [[globalInt_USR]] | Ref,Read,RelCont | rel: 1
      // CHECK-NEXT: RelCont | instance-property/Swift | z
    }
    return Inner().x + globalInt
    // CHECK: [[@LINE-1]]:24 | variable/Swift | globalInt | [[globalInt_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK-NEXT: RelCont | instance-property/Swift | z
  })
  // CHECK: [[@LINE-17]]:4 | constructor/Swift | init(body:) | [[WrapperBodyInit_USR]] | Ref,Call,RelCont | rel: 1
  public var z: Int
  // CHECK: [[@LINE-1]]:14 | instance-property/Swift | z | [[z_USR:.*]] | Def,RelChild | rel: 1

  @Wrapper
  public var `escaped identifier`: Int = globalInt
  // CHECK: [[@LINE-1]]:14 | instance-property/Swift | escaped identifier | [[escaped_identifier_USR:.*]] | Def,RelChild | rel: 1

  func backingUse() {
    _ = _y.wrappedValue + _z.wrappedValue + x + _x.wrappedValue + $y.item
    // CHECK: [[@LINE-1]]:10 | instance-property/Swift | y | [[y_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-2]]:12 | instance-property/Swift | wrappedValue | [[wrappedValue_USR:.*]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-3]]:28 | instance-property/Swift | z | [[z_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-4]]:45 | instance-property/Swift | x | [[x_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-5]]:50 | instance-property/Swift | x | [[x_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-6]]:68 | instance-property/Swift | y | [[y_USR]] | Ref,Read,RelCont | rel: 1
    _ = `_escaped identifier`.wrappedValue + `$escaped identifier`.item
    // CHECK: [[@LINE-1]]:11 | instance-property/Swift | escaped identifier | [[escaped_identifier_USR]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-2]]:31 | instance-property/Swift | wrappedValue | [[wrappedValue_USR:.*]] | Ref,Read,RelCont | rel: 1
    // CHECK: [[@LINE-3]]:48 | instance-property/Swift | escaped identifier | [[escaped_identifier_USR]] | Ref,Read,RelCont | rel: 1
  }
}

func useMemberwiseInits(i: Int) {
  _ = HasWrappers(x: i)
  _ = HasWrappers(y: Wrapper(initialValue: i))
}
