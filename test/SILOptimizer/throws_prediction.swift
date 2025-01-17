// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -enable-throws-prediction \
// RUN:   -sil-verify-all -module-name=test -emit-sil \
// RUN:       | %FileCheck --check-prefix CHECK-SIL %s

// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -enable-throws-prediction \
// RUN:   -sil-verify-all -module-name=test -emit-irgen \
// RUN:       | %FileCheck --check-prefix CHECK-IR %s

// RUN: %target-swift-frontend %s \
// RUN:   -target %target-swift-5.1-abi-triple \
// RUN:   -disable-throws-prediction \
// RUN:   -sil-verify-all -module-name=test -emit-sil \
// RUN:       | %FileCheck --check-prefix CHECK-DISABLED %s

// UNSUPPORTED: CPU=armv7k || CPU=arm64_32

// CHECK-DISABLED-NOT: normal_count

enum MyError: Error { case err }

func throwy1() throws {}
func throwy2() throws(MyError) { }
func throwy3() async throws -> Int { 0 }
func throwy4() async throws(MyError) -> Int { 1 }

// CHECK-SIL-LABEL: sil hidden @$s4test0A13TryPredictionyySbF
// CHECK-SIL: try_apply {{.*}} @error any Error{{.*}} !normal_count(1999) !error_count(0)
// CHECK-SIL: try_apply {{.*}} @error MyError{{.*}} !normal_count(1999) !error_count(0)

// CHECK-IR-LABEL: define hidden swiftcc void @"$s4test0A13TryPredictionyySbF"
// CHECK-IR:   call swiftcc void @"$s4test7throwy1yyKF"
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]

// CHECK-IR:   call swiftcc void @"$s4test7throwy2yyAA7MyErrorOYKF"
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
func testTryPrediction(_ b: Bool) {
  do {
    try throwy1()
    try throwy2()
  } catch {
    print("hi")
  }
}

// CHECK-SIL-LABEL: sil hidden @$s4test0A21AsyncThrowsPredictionSiyYaF
// CHECK-SIL: function_ref @$s4test7throwy3SiyYaKF
// CHECK-SIL: try_apply {{.*}} @error any Error{{.*}} !normal_count(1999) !error_count(0)

// CHECK-IR-LABEL: define hidden swifttailcc void @"$s4test0A21AsyncThrowsPredictionSiyYaF"
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
func testAsyncThrowsPrediction() async -> Int {
    if let x = try? await throwy3() {
        return x
    }
    return 1337
}

// CHECK-SIL-LABEL: sil hidden @$s4test0A28Async_TYPED_ThrowsPredictionSiyYaAA7MyErrorOYKF
// CHECK-SIL: try_apply {{.*}} @error MyError{{.*}} !normal_count(1999) !error_count(0)
// CHECK-SIL: try_apply {{.*}} @error MyError{{.*}} !normal_count(1999) !error_count(0)
// CHECK-SIL: try_apply {{.*}} @error MyError{{.*}} !normal_count(1999) !error_count(0)

// CHECK-IR-LABEL: define hidden swifttailcc void @"$s4test0A28Async_TYPED_ThrowsPredictionSiyYaAA7MyErrorOYKF"
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
//
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
//
// CHECK-IR:   [[ERROR_PTR:%.*]] = load ptr, ptr %swifterror
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr [[ERROR_PTR]], null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
func testAsync_TYPED_ThrowsPrediction() async throws(MyError) -> Int {
    let x = try await throwy4()
    let y = try await throwy4()
    let z = try await throwy4()
    return x + y + z
}


func getRandom(_ b: Bool) throws -> Int {
    if b {
        return Int.random(in: 0..<1024)
    } else {
        throw MyError.err
    }
}

// CHECK-SIL-LABEL: sil hidden @$s4test20sequenceOfNormalTrysySiSb_S2btKF
// CHECK-SIL: try_apply {{.*}} @error any Error{{.*}} !normal_count(1999) !error_count(0)
// CHECK-SIL: try_apply {{.*}} @error any Error{{.*}} !normal_count(1999) !error_count(0)
// CHECK-SIL: try_apply {{.*}} @error any Error{{.*}} !normal_count(1999) !error_count(0)

// CHECK-IR-LABEL: define hidden swiftcc i64 @"$s4test20sequenceOfNormalTrysySiSb_S2btKF"
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr {{%.*}}, null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
//
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr {{%.*}}, null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
//
// CHECK-IR:   [[HAVE_ERROR:%.*]] = icmp ne ptr {{%.*}}, null
// CHECK-IR:   br i1 [[HAVE_ERROR]], {{.*}} !prof [[PREFER_FALSE:![0-9]+]]
func sequenceOfNormalTrys(_ b1: Bool,
                          _ b2: Bool,
                          _ b3: Bool) throws -> Int {
    let x = try getRandom(b1)
    let y = try getRandom(b2)
    let z = try getRandom(b3)
    return x + y + z
}

// CHECK-IR: [[PREFER_FALSE]] = !{!"branch_weights", i32 1, i32 2000}
