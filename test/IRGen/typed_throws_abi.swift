// RUN: %target-swift-frontend -primary-file %s -emit-irgen -enable-library-evolution | %FileCheck %s

// REQUIRES: PTRSIZE=64

struct Empty: Error {}

struct OneWord: Error {
    let x = 1
}

struct Impl: P {
    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2f0yySbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret void
    // CHECK: }
    func f0(_ b: Bool) throws(Empty) {
        guard b else {
            throw Empty()
        }
    }

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2f1ySiSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret i64 1
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret i64 undef
    // CHECK: }
    func f1(_ b: Bool) throws(Empty) -> Int {
        guard b else {
            throw Empty()
        }
        return 1
    }

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2f2ySi_SitSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } { i64 1, i64 2 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64 } undef
    // CHECK: }
    func f2(_ b: Bool) throws(Empty) -> (Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f3ySi_S2itSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 3 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } undef
    // CHECK: }
    func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f4ySi_S3itSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64, i64 } { i64 1, i64 2, i64 3, i64 4 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64, i64 } undef
    // CHECK: }
    func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2f5ySi_S4itSbAA5EmptyVYKF"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr %.elt._value, align 8
    // CHECK:   store i64 2, ptr %.elt1._value, align 8
    // CHECK:   store i64 3, ptr %.elt2._value, align 8
    // CHECK:   store i64 4, ptr %.elt3._value, align 8
    // CHECK:   store i64 5, ptr %.elt4._value, align 8
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"() #12
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %3, align 8
    // CHECK:   ret void
    // CHECK: }
    func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4, 5)
    }

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2g0yySbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret i64 undef
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func g0(_ b: Bool) throws(OneWord) {
        guard b else {
            throw OneWord()
        }
    }

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2g1ySiSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret i64 1
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func g1(_ b: Bool) throws(OneWord) -> Int {
        guard b else {
            throw OneWord()
        }
        return 1
    }

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2g2ySi_SitSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } { i64 1, i64 2 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g3ySi_S2itSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 3 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g4ySi_S3itSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64, i64 } { i64 1, i64 2, i64 3, i64 4 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2g5ySi_S4itSbAA7OneWordVYKF"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4) #0 {
    // CHECK: entry:
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr {{%.*}}, align 8
    // CHECK:   store i64 2, ptr {{%.*}}, align 8
    // CHECK:   store i64 3, ptr {{%.*}}, align 8
    // CHECK:   store i64 4, ptr {{%.*}}, align 8
    // CHECK:   store i64 5, ptr {{%.*}}, align 8
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %3, align 8
    // CHECK:   ret void
    // CHECK: }
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4, 5)
    }
}

struct ImplAsync: PAsync {

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f0yySbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1) #0 {
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f0(_ b: Bool) async throws(Empty) {
        guard b else {
            throw Empty()
        }
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f1ySiSbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   %16 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   %23 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f1(_ b: Bool) async throws(Empty) -> Int {
        guard b else {
            throw Empty()
        }
        return 1
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f2ySi_SitSbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   %16 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   %23 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f2(_ b: Bool) async throws(Empty) -> (Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f3ySi_S2itSbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   %16 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   %23 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f3(_ b: Bool) async throws(Empty) -> (Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f4ySi_S3itSbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   %16 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, i64 4, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   %23 = call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f4(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f5ySi_S4itSbYaAA5EmptyVYKF"(ptr noalias nocapture %0, ptr swiftasync %1, i1 %2, ptr %3) #0 {
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %2, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr {{%.*}}, align 8
    // CHECK:   store i64 2, ptr {{%.*}}, align 8
    // CHECK:   store i64 3, ptr {{%.*}}, align 8
    // CHECK:   store i64 4, ptr {{%.*}}, align 8
    // CHECK:   store i64 5, ptr {{%.*}}, align 8
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f5(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4, 5)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g0yySbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g0(_ b: Bool) async throws(OneWord) {
        guard b else {
            throw OneWord()
        }
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g1ySiSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g1(_ b: Bool) async throws(OneWord) -> Int {
        guard b else {
            throw OneWord()
        }
        return 1
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g2ySi_SitSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g2(_ b: Bool) async throws(OneWord) -> (Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g3ySi_S2itSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g4ySi_S3itSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, i64 4, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g5ySi_S4itSbYaAA7OneWordVYKF"(ptr noalias nocapture %0, ptr swiftasync %1, i1 %2, ptr %3) #0 {
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi7OneWordV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %2, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr {{%.*}}, align 8
    // CHECK:   store i64 2, ptr {{%.*}}, align 8
    // CHECK:   store i64 3, ptr {{%.*}}, align 8
    // CHECK:   store i64 4, ptr {{%.*}}, align 8
    // CHECK:   store i64 5, ptr {{%.*}}, align 8
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi7OneWordVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi7OneWordVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw OneWord()
        }
        return (1, 2, 3, 4, 5)
    }
}

protocol P {
    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f0yySbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret void
    // CHECK: success:
    // CHECK:   ret void
    // CHECK: }
    func f0(_ b: Bool) throws(Empty)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2f1ySiSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 undef
    // CHECK: success:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func f1(_ b: Bool) throws(Empty) -> Int

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2f2ySi_SitSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func f2(_ b: Bool) throws(Empty) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2f3ySi_S2itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2f4ySi_S3itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f5ySi_S4itSbAA5EmptyVYKFTj"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2g0yySbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: success:
    // CHECK:   ret i64 undef
    // CHECK: }
    func g0(_ b: Bool) throws(OneWord)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2g1ySiSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: success:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func g1(_ b: Bool) throws(OneWord) -> Int

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2g2ySi_SitSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2g3ySi_S2itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2g4ySi_S3itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2g5ySi_S4itSbAA7OneWordVYKFTj"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int)
}

protocol PAsync {
    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f0yySbYaAA5EmptyVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f0(_ b: Bool) async throws(Empty)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f1ySiSbYaAA5EmptyVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f1(_ b: Bool) async throws(Empty) -> Int

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f2ySi_SitSbYaAA5EmptyVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f2(_ b: Bool) async throws(Empty) -> (Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f3ySi_S2itSbYaAA5EmptyVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f3(_ b: Bool) async throws(Empty) -> (Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f4ySi_S3itSbYaAA5EmptyVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, i64 undef, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f4(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f5ySi_S4itSbYaAA5EmptyVYKFTj"(ptr noalias nocapture %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f5(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int, Int)

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g0yySbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // // CHECK: failure:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: success:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr [[ERROR]])
    // // CHECK: }
    func g0(_ b: Bool) async throws(OneWord)

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g1ySiSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // // CHECK: failure:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: success:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: }
    func g1(_ b: Bool) async throws(OneWord) -> Int

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g2ySi_SitSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // // CHECK: failure:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: success:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: }
    func g2(_ b: Bool) async throws(OneWord) -> (Int, Int)

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g3ySi_S2itSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // // CHECK: failure:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}} ptr [[ERROR]])
    // // CHECK: success:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: }
    func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int)

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g4ySi_S3itSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // // CHECK: failure:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: success:
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // // CHECK: }
    func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int)

    // // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g5ySi_S4itSbYaAA7OneWordVYKFTj"(ptr noalias nocapture %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // // CHECK:   %swifterror = alloca swifterror ptr
    // // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // // CHECK:   store ptr null, ptr %swifterror
    // // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // // CHECK: }
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int)
}
