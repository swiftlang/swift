// Check that the error box metadata's destroy function is signed for its slot
// (IA key, address-diversified, 0xbbbf), as the release path authenticates it.

// RUN: %target-swift-frontend -emit-ir -O -wmo -parse-as-library -enable-experimental-feature Embedded %s | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: swift_feature_Embedded

struct MyError: Error {}

@inline(never)
func fail() throws(any Error) {
  throw MyError()
}

public func test() -> Bool {
  do {
    try fail()
    return false
  } catch {
    return true
  }
}

// CHECK-NOT: store ptr @_swift_embedded_error_box_destroy.ptrauth
// CHECK:     [[DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 ptrtoint (ptr getelementptr inbounds {{.*}}(i8, ptr @_swift_embedded_error_metadata_storage, i64 8) to i64), i64 48063)
// CHECK:     call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @_swift_embedded_error_box_destroy to i64), i32 0, i64 [[DISC]])
