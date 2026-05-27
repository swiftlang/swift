// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo -o - | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func unsafeWriteArray<T, R>(_ elementType: R.Type, array: inout T, index n: Int, value: R) {
  precondition(_isPOD(elementType))
  precondition(_isPOD(type(of: array)))

  return withUnsafeMutableBytes(of: &array) { ptr in
    let buffer = ptr.bindMemory(to: R.self)
    precondition(n >= 0)
    precondition(n < buffer.count)
    buffer[n] = value
  }
}

public func test() {
  var args: (Int, Int, Int, Int) = (0, 0, 0, 0)
  let n = 2
  let value = 42
  unsafeWriteArray(type(of: args.0), array: &args, index: n, value: value)
}


func getType<T>(of: T.Type) -> Any.Type {
  return T.self
}

// Type metadata symbols
// CHECK: @"$eytWV" = linkonce_odr hidden constant
// CHECK: @"$eytMf" = linkonce_odr hidden constant
// CHECK: @"$eSi_SdtWV" = linkonce_odr hidden constant
// CHECK: @"$eSi_SdtMf" = linkonce_odr hidden constant
// CHECK: @"$eSd_SdtmMf" = linkonce_odr hidden constant
// CHECK: @"$es5Error_pMf" = linkonce_odr hidden constant

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$e6typeof7getType2ofypXpxm_tlFyt_Ttg5"
// CHECK: ptr @"$eytMf"

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$e6typeof7getType2ofypXpxm_tlFSi_Sdt_Ttg5"
// CHECK: ptr @"$eSi_SdtMf"

// CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$e6typeof7getType2ofypXpxm_tlFSd_Sdtm_Ttg5"()
// CHECK: ptr @"$eSd_SdtmMf"

// CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$e6typeof7getType2ofypXpxm_tlFs5Error_p_Ttg5"()
// CHECK: ptr @"$es5Error_pMf"

public func readTypes() {
  _ = getType(of: Void.self)
  _ = getType(of: (Int, Double).self)
  _ = getType(of: (Double, Double).Type.self)
  _ = getType(of: (any Error).self)
}

public class C { }
class D: C { }

func realGetType<T>(of value: T) -> Any.Type {
  return type(of: value)
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$e6typeof11realGetType2ofypXpx_tlFSd_Tg5"(double %0)
// CHECK: ptr @"$eSdMf"

// CHECK-LABEL: define linkonce_odr hidden swiftcc ptr @"$e6typeof11realGetType2ofypXpx_tlFAA1CC_Tg5"(ptr %0)
// CHECK: [[ALLOCA:%.*]] = alloca ptr
// CHECK: store ptr %0, ptr [[ALLOCA]]
// CHECK: [[VTABLE:%.*]] = load ptr, ptr [[ALLOCA]]
// CHECK: [[METADATA:%.*]] = load ptr, ptr [[VTABLE]]
// CHECK: ret ptr [[METADATA]]

public func readTypesWithTypeOf(c: C) {
  _ = realGetType(of: 3.1415)
  _ = realGetType(of: c)
}
