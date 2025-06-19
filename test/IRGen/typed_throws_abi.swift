// RUN: %target-swift-frontend -primary-file %s -emit-irgen -enable-library-evolution | %FileCheck %s

// XFAIL: CPU=arm64e
// REQUIRES: PTRSIZE=64

struct Empty: Error {}

struct OneWord: Error {
    let x = 0
}

struct TwoWords: Error {
    let x = 0
    let y = 0
}

struct ThreeWords: Error {
    let x = 0
    let y = 0
    let z = 0
}

struct Impl: P {
    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2f0yySbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret void
    // CHECK: }
    func f0(_ b: Bool) throws(Empty) {
        guard b else {
            throw Empty()
        }
    }

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2f1ySiSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret i64 1
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
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

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2f2ySi_SitSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } { i64 1, i64 2 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
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

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f3ySi_S2itSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 3 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
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

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f4ySi_S3itSbAA5EmptyVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64, i64 } { i64 1, i64 2, i64 3, i64 4 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
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

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2f5ySi_S4itSbAA5EmptyVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4)
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
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
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

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2g0yySbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
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

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi4ImplV2g1ySiSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
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

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2g2ySi_SitSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
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

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g3ySi_S2itSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
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

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g4ySi_S3itSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
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

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2g5ySi_S4itSbAA7OneWordVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4)
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

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h0yySbAA8TwoWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } undef
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func h0(_ b: Bool) throws(TwoWords) {
        guard b else {
            throw TwoWords()
        }
    }

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h1ySiSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } { i64 1, i64 undef }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func h1(_ b: Bool) throws(TwoWords) -> Int {
        guard b else {
            throw TwoWords()
        }
        return 1
    }

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h2ySi_SitSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64 } { i64 1, i64 2 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func h2(_ b: Bool) throws(TwoWords) -> (Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2h3ySi_S2itSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 3 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func h3(_ b: Bool) throws(TwoWords) -> (Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2h4ySi_S3itSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64, i64 } { i64 1, i64 2, i64 3, i64 4 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func h4(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2h5ySi_S4itSbAA8TwoWordsVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4)
    // CHECK: entry:
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr {{%.*}}, align 8
    // CHECK:   store i64 2, ptr {{%.*}}, align 8
    // CHECK:   store i64 3, ptr {{%.*}}, align 8
    // CHECK:   store i64 4, ptr {{%.*}}, align 8
    // CHECK:   store i64 5, ptr {{%.*}}, align 8
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %3, align 8
    // CHECK:   ret void
    // CHECK: }
    func h5(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4, 5)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i0yySbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } undef
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i0(_ b: Bool) throws(ThreeWords) {
        guard b else {
            throw ThreeWords()
        }
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i1ySiSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 undef, i64 undef }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i1(_ b: Bool) throws(ThreeWords) -> Int {
        guard b else {
            throw ThreeWords()
        }
        return 1
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i2ySi_SitSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 undef }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i2(_ b: Bool) throws(ThreeWords) -> (Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i3ySi_S2itSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64 } { i64 1, i64 2, i64 3 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i3(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i4ySi_S3itSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV
    // CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   ret { i64, i64, i64, i64 } { i64 1, i64 2, i64 3, i64 4 }
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i4(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi4ImplV2i5ySi_S4itSbAA10ThreeWordsVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4)
    // CHECK: entry:
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   store i64 1, ptr {{%.*}}, align 8
    // CHECK:   store i64 2, ptr {{%.*}}, align 8
    // CHECK:   store i64 3, ptr {{%.*}}, align 8
    // CHECK:   store i64 4, ptr {{%.*}}, align 8
    // CHECK:   store i64 5, ptr {{%.*}}, align 8
    // CHECK:   ret void
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %3, align 8
    // CHECK:   ret void
    // CHECK: }
    func i5(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4, 5)
    }
}

// CHECK: define hidden swiftcc i1 @"$s16typed_throws_abi11callImpl_f0ySbAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   call swiftcc void @"$s16typed_throws_abi4ImplV2f0yySbAA5EmptyVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL:%.*]] = phi i1 [ false, %[[SET_ERROR]] ], [ true, %[[SUCCESS]] ]
// CHECK:   ret i1 [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f0(_ impl: Impl, _ b: Bool) -> Bool {
    do {
        try impl.f0(b)
        return true
    } catch {
        return false
    }
}

// CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi11callImpl_f1ySiAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc i64 @"$s16typed_throws_abi4ImplV2f1ySiSbAA5EmptyVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES:%.*]] = phi i64 [ [[CALL_RES]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES]], %[[SUCCESS]] ]
// CHECK:   ret i64 [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f1(_ impl: Impl, _ b: Bool) -> Int {
    do {
        return try impl.f1(b)
    } catch {
        return 0
    }
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi11callImpl_f2ySi_SitAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2f2ySi_SitSbAA5EmptyVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = insertvalue { i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64 } [[RETVAL2]], i64 [[RETVAL1]], 1
// CHECK:   ret { i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f2(_ impl: Impl, _ b: Bool) -> (Int, Int) {
    do {
        return try impl.f2(b)
    } catch {
        return (0, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_f3ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f3ySi_S2itSbAA5EmptyVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f3(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        return try impl.f3(b)
    } catch {
        return (0, 0, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi11callImpl_f4ySi_S3itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2f4ySi_S3itSbAA5EmptyVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 3
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL5:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL6:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL5]], i64 [[RETVAL2]], 2
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL6]], i64 [[RETVAL3]], 3
// CHECK:   ret { i64, i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f4(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int) {
    do {
        return try impl.f4(b)
    } catch {
        return (0, 0, 0, 0)
    }
}

// CHECK: define hidden swiftcc void @"$s16typed_throws_abi11callImpl_f5ySi_S4itAA0E0V_SbtF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   %swifterror1 = alloca ptr, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   call swiftcc void @"$s16typed_throws_abi4ImplV2f5ySi_S4itSbAA5EmptyVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %call.aggresult, i1 %1, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror, ptr %swifterror1)
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr {{%.elt.*}}, align 8
// CHECK:   ret void
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_f5(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int, Int) {
    do {
        return try impl.f5(b)
    } catch {
        return (0, 0, 0, 0, 0)
    }
}

// CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi11callImpl_g0ySiAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc i64 @"$s16typed_throws_abi4ImplV2g0yySbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ [[FAIL_RES:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   ret i64 [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES]] = phi i64 [ [[CALL_RES]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g0(_ impl: Impl, _ b: Bool) -> Int {
    do {
        try impl.g0(b)
        return 0
    } catch {
        return error.x
    }
}

// CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi11callImpl_g1ySiAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc i64 @"$s16typed_throws_abi4ImplV2g1ySiSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES:%.*]] = phi i64 [ [[CALL_RES]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ [[FAIL_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES]], %[[SUCCESS]] ]
// CHECK:   ret i64 [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES]] = phi i64 [ [[CALL_RES]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g1(_ impl: Impl, _ b: Bool) -> Int {
    do {
        return try impl.g1(b)
    } catch {
        return error.x
    }
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi11callImpl_g2ySi_SitAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2g2ySi_SitSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = insertvalue { i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64 } [[RETVAL2]], i64 [[RETVAL1]], 1
// CHECK:   ret { i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g2(_ impl: Impl, _ b: Bool) -> (Int, Int) {
    do {
        return try impl.g2(b)
    } catch {
        return (error.x, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_g3ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g3ySi_S2itSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g3(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        return try impl.g3(b)
    } catch {
        return (error.x, 0, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi11callImpl_g4ySi_S3itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2g4ySi_S3itSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 3
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL5:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL6:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL5]], i64 [[RETVAL2]], 2
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL6]], i64 [[RETVAL3]], 3
// CHECK:   ret { i64, i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g4(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int) {
    do {
        return try impl.g4(b)
    } catch {
        return (error.x, 0, 0, 0)
    }
}

// CHECK: define hidden swiftcc void @"$s16typed_throws_abi11callImpl_g5ySi_S4itAA0E0V_SbtF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   %swifterror1 = alloca %T16typed_throws_abi7OneWordV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   call swiftcc void @"$s16typed_throws_abi4ImplV2g5ySi_S4itSbAA7OneWordVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %call.aggresult, i1 %1, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror, ptr %swifterror1)
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror1.x = getelementptr inbounds{{.*}} %T16typed_throws_abi7OneWordV, ptr %swifterror1, i32 0, i32 0
// CHECK:   %swifterror1.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror1.x, i32 0, i32 0
// CHECK:   [[CALL_ERROR_RES:%.*]] = load i64, ptr %swifterror1.x._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr {{%.elt.*}}, align 8
// CHECK:   ret void
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_ERROR_RES]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_g5(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int, Int) {
    do {
        return try impl.g5(b)
    } catch {
        return (error.x, 0, 0, 0, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi11callImpl_h0ySi_SitAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h0yySbAA8TwoWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = insertvalue { i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64 } [[RETVAL2]], i64 [[RETVAL1]], 1
// CHECK:   ret { i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h0(_ impl: Impl, _ b: Bool) -> (Int, Int) {
    do {
        try impl.h0(b)
        return (0, 0)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi11callImpl_h1ySi_SitAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h1ySiSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = insertvalue { i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64 } [[RETVAL2]], i64 [[RETVAL1]], 1
// CHECK:   ret { i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h1(_ impl: Impl, _ b: Bool) -> (Int, Int) {
    do {
        return try (impl.h1(b), 0)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi11callImpl_h2ySi_SitAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64 } @"$s16typed_throws_abi4ImplV2h2ySi_SitSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } [[CALL_RES]], 1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = insertvalue { i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64 } [[RETVAL2]], i64 [[RETVAL1]], 1
// CHECK:   ret { i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h2(_ impl: Impl, _ b: Bool) -> (Int, Int) {
    do {
        return try impl.h2(b)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_h3ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2h3ySi_S2itSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h3(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        return try impl.h3(b)
    } catch {
        return (error.x, error.y, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi11callImpl_h4ySi_S3itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2h4ySi_S3itSbAA8TwoWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 3
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL5:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL6:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL5]], i64 [[RETVAL2]], 2
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL6]], i64 [[RETVAL3]], 3
// CHECK:   ret { i64, i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h4(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int) {
    do {
        return try impl.h4(b)
    } catch {
        return (error.x, error.y, 0, 0)
    }
}

// CHECK: define hidden swiftcc void @"$s16typed_throws_abi11callImpl_h5ySi_S4itAA0E0V_SbtF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   %swifterror1 = alloca %T16typed_throws_abi8TwoWordsV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   call swiftcc void @"$s16typed_throws_abi4ImplV2h5ySi_S4itSbAA8TwoWordsVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %call.aggresult, i1 %1, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror, ptr %swifterror1)
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror1.x = getelementptr inbounds{{.*}} %T16typed_throws_abi8TwoWordsV, ptr %swifterror1, i32 0, i32 0
// CHECK:   %swifterror1.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror1.x, i32 0, i32 0
// CHECK:   [[CALL_ERROR_RES0:%.*]] = load i64, ptr %swifterror1.x._value, align 8
// CHECK:   [[CALL_ERROR_RES1:%.*]] = load i64, ptr %swifterror1.y._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr {{%.elt.*}}, align 8
// CHECK:   ret void
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_ERROR_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_ERROR_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_h5(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int, Int) {
    do {
        return try impl.h5(b)
    } catch {
        return (error.x, error.y, 0, 0, 0)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_i0ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i0yySbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[FAIL_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[FAIL_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i0(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        try impl.i0(b)
        return (0, 0, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_i1ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i1ySiSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[FAIL_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[FAIL_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i1(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        return try (impl.i1(b), 0, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_i2ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i2ySi_SitSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[FAIL_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[FAIL_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i2(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        let res = try impl.i2(b)
        return (res.0, res.1, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi11callImpl_i3ySi_S2itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i3ySi_S2itSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[FAIL_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL3]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL2]], 2
// CHECK:   ret { i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[FAIL_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i3(_ impl: Impl, _ b: Bool) -> (Int, Int, Int) {
    do {
        return try impl.i3(b)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi11callImpl_i4ySi_S3itAA0E0V_SbtF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi4ImplV2i4ySi_S3itSbAA10ThreeWordsVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } [[CALL_RES]], 3
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET:.*]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[FAIL_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[FAIL_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[FAIL_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, i64, i64, i64 } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL5:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL4]], i64 [[RETVAL1]], 1
// CHECK:   [[RETVAL6:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL5]], i64 [[RETVAL2]], 2
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, i64, i64, i64 } [[RETVAL6]], i64 [[RETVAL3]], 3
// CHECK:   ret { i64, i64, i64, i64 } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[FAIL_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[FAIL_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[FAIL_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i4(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int) {
    do {
        return try impl.i4(b)
    } catch {
        return (error.x, error.y, error.z, 0)
    }
}

// CHECK: define hidden swiftcc void @"$s16typed_throws_abi11callImpl_i5ySi_S4itAA0E0V_SbtF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   %swifterror1 = alloca %T16typed_throws_abi10ThreeWordsV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   call swiftcc void @"$s16typed_throws_abi4ImplV2i5ySi_S4itSbAA10ThreeWordsVYKF"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %call.aggresult, i1 %1, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror, ptr %swifterror1)
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr {{%call.aggresult.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror1.x = getelementptr inbounds{{.*}} %T16typed_throws_abi10ThreeWordsV, ptr %swifterror1, i32 0, i32 0
// CHECK:   %swifterror1.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror1.x, i32 0, i32 0
// CHECK:   [[CALL_ERROR_RES0:%.*]] = load i64, ptr %swifterror1.x._value, align 8
// CHECK:   [[CALL_ERROR_RES1:%.*]] = load i64, ptr %swifterror1.y._value, align 8
// CHECK:   [[CALL_ERROR_RES2:%.*]] = load i64, ptr %swifterror1.z._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr {{%.elt.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr {{%.elt.*}}, align 8
// CHECK:   ret void
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_ERROR_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_ERROR_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_ERROR_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callImpl_i5(_ impl: Impl, _ b: Bool) -> (Int, Int, Int, Int, Int) {
    do {
        return try impl.i5(b)
    } catch {
        return (error.x, error.y, error.z, 0, 0)
    }
}

@available(SwiftStdlib 6.0, *)
struct ImplAsync: PAsync {

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f0yySbYaAA5EmptyVYKF"(ptr swiftasync %0, i1 %1)
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
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr inttoptr (i64 1 to ptr))
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
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
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
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
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
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, i64 4, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc void @"$s16typed_throws_abi5EmptyVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi5EmptyVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias undef, ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr }>, ptr @"$s16typed_throws_abi5EmptyVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, i64 undef, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func f4(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int) {
        guard b else {
            throw Empty()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2f5ySi_S4itSbYaAA5EmptyVYKF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr %3)
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

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2g5ySi_S4itSbYaAA7OneWordVYKF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr %3)
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

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h0yySbYaAA8TwoWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h0(_ b: Bool) async throws(TwoWords) {
        guard b else {
            throw TwoWords()
        }
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h1ySiSbYaAA8TwoWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h1(_ b: Bool) async throws(TwoWords) -> Int {
        guard b else {
            throw TwoWords()
        }
        return 1
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h2ySi_SitSbYaAA8TwoWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h2(_ b: Bool) async throws(TwoWords) -> (Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h3ySi_S2itSbYaAA8TwoWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h3(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h4ySi_S3itSbYaAA8TwoWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, i64 4, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}} ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h4(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2h5ySi_S4itSbYaAA8TwoWordsVYKF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr %3)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi8TwoWordsV, align 8
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
    // CHECK:   call swiftcc { i64, i64 } @"$s16typed_throws_abi8TwoWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi8TwoWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32 }>, ptr @"$s16typed_throws_abi8TwoWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func h5(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw TwoWords()
        }
        return (1, 2, 3, 4, 5)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i0yySbYaAA10ThreeWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i0(_ b: Bool) async throws(ThreeWords) {
        guard b else {
            throw ThreeWords()
        }
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i1ySiSbYaAA10ThreeWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 undef, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i1(_ b: Bool) async throws(ThreeWords) -> Int {
        guard b else {
            throw ThreeWords()
        }
        return 1
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i2ySi_SitSbYaAA10ThreeWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 undef, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i2(_ b: Bool) async throws(ThreeWords) -> (Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i3ySi_S2itSbYaAA10ThreeWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i3(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i4ySi_S3itSbYaAA10ThreeWordsVYKF"(ptr swiftasync %0, i1 %1)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
    // CHECK: [[SUCCESS]]:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 1, i64 2, i64 3, i64 4, ptr null)
    // CHECK:   unreachable
    // CHECK: [[FAIL]]:
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i4(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4)
    }

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi9ImplAsyncV2i5ySi_S4itSbYaAA10ThreeWordsVYKF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr %3)
    // CHECK:   [[ERROR:%.*]] = alloca %T16typed_throws_abi10ThreeWordsV, align 8
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
    // CHECK:   call swiftcc { i64, i64, i64 } @"$s16typed_throws_abi10ThreeWordsVACycfC"()
    // CHECK:   [[ERROR_WITNESS:%.*]] = call ptr @"$s16typed_throws_abi10ThreeWordsVACs5ErrorAAWl"()
    // CHECK:   call swiftcc void @swift_willThrowTyped(ptr noalias [[ERROR]], ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i32, i32, i32, [4 x i8] }>, ptr @"$s16typed_throws_abi10ThreeWordsVMf", i32 0, i32 2), ptr [[ERROR_WITNESS]])
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr inttoptr (i64 1 to ptr))
    // CHECK:   unreachable
    // CHECK: }
    func i5(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int, Int) {
        guard b else {
            throw ThreeWords()
        }
        return (1, 2, 3, 4, 5)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f0ySbAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_f0ySbAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, ptr } [[CALL_RES]], 1
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL:%.*]] = phi i1 [ false, %[[SET_ERROR]] ], [ true, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i1 [[RETVAL]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f0(_ impl: ImplAsync, _ b: Bool) async -> Bool {
    do {
        try await impl.f0(b)
        return true
    } catch {
        return false
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f1ySiAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_f1ySiAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 2
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f1(_ impl: ImplAsync, _ b: Bool) async -> Int {
    do {
        return try await impl.f1(b)
    } catch {
        return 0
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f2ySi_SitAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_f2ySi_SitAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } {{%.*}}, 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, ptr } [[CALL_RES]], 3
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f2(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int) {
    do {
        return try await impl.f2(b)
    } catch {
        return (0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f3ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_f3ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f3(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        return try await impl.f3(b)
    } catch {
        return (0, 0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f4ySi_S3itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_f4ySi_S3itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 3
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, i64, ptr } [[CALL_RES]], 5
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]], i64 [[RETVAL3]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f4(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int) {
    do {
        return try await impl.f4(b)
    } catch {
        return (0, 0, 0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_f5ySi_S4itAA0eF0V_SbtYaF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2)
// CHECK:   %swifterror = alloca ptr, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   %swifterror1 = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 1, ptr @"$s16typed_throws_abi16callImplAsync_f5ySi_S4itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr %call.aggresult, ptr {{%.*}}, i1 %2, ptr %swifterror)
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, ptr } [[CALL_RES]], 1
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror1, align 8
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror1, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr %.elt{{.*}}, align 8
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}})
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_f5(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int, Int) {
    do {
        return try await impl.f5(b)
    } catch {
        return (0, 0, 0, 0, 0)
    }
}


// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g0ySiAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_g0ySiAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 2
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ 1, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g0(_ impl: ImplAsync, _ b: Bool) async -> Int {
    do {
        try await impl.g0(b)
        return 1
    } catch {
        return error.x
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g1ySiAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_g1ySiAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, ptr } [[CALL_RES]], 2
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g1(_ impl: ImplAsync, _ b: Bool) async -> Int {
    do {
        return try await impl.g1(b)
    } catch {
        return error.x
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g2ySi_SitAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_g2ySi_SitAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } {{%.*}}, 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, ptr } [[CALL_RES]], 3
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g2(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int) {
    do {
        return try await impl.g2(b)
    } catch {
        return (error.x, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g3ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_g3ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g3(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        return try await impl.g3(b)
    } catch {
        return (error.x, 0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g4ySi_S3itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_g4ySi_S3itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 3
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, i64, ptr } [[CALL_RES]], 5
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]], i64 [[RETVAL3]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g4(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int) {
    do {
        return try await impl.g4(b)
    } catch {
        return (error.x, 0, 0, 0)
    }
}


// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_g5ySi_S4itAA0eF0V_SbtYaF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2)
// CHECK:   %swifterror = alloca %T16typed_throws_abi7OneWordV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   %swifterror1 = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 1, ptr @"$s16typed_throws_abi16callImplAsync_g5ySi_S4itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr %call.aggresult, ptr {{%.*}}, i1 %2, ptr %swifterror)
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, ptr } [[CALL_RES]], 1
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror1, align 8
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror1, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror.x = getelementptr inbounds{{.*}} %T16typed_throws_abi7OneWordV, ptr %swifterror, i32 0, i32 0
// CHECK:   %swifterror.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror.x, i32 0, i32 0
// CHECK:   [[ERROR_X:%.*]] = load i64, ptr %swifterror.x._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr %.elt{{.*}}, align 8
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}})
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:    [[ERROR_RES0]] = phi i64 [ [[ERROR_X]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_g5(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int, Int) {
    do {
        return try await impl.g5(b)
    } catch {
        return (error.x, 0, 0, 0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h0ySi_SitAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_h0ySi_SitAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } {{%.*}}, 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, ptr } [[CALL_RES]], 3
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h0(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int) {
    do {
        try await impl.h0(b)
        return (0, 0)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h1ySi_SitAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_h1ySi_SitAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } {{%.*}}, 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, ptr } [[CALL_RES]], 3
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h1(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int) {
    do {
        return try await (impl.h1(b), 0)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h2ySi_SitAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_h2ySi_SitAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64 } {{%.*}}, 1
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, ptr } [[CALL_RES]], 3
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h2(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int) {
    do {
        return try await impl.h2(b)
    } catch {
        return (error.x, error.y)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h3ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_h3ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h3(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        return try await impl.h3(b)
    } catch {
        return (error.x, error.y, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h4ySi_S3itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_h4ySi_S3itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 3
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, i64, ptr } [[CALL_RES]], 5
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]], i64 [[RETVAL3]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h4(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int) {
    do {
        return try await impl.h4(b)
    } catch {
        return (error.x, error.y, 0, 0)
    }
}


// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_h5ySi_S4itAA0eF0V_SbtYaF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2)
// CHECK:   %swifterror = alloca %T16typed_throws_abi8TwoWordsV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   %swifterror1 = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 1, ptr @"$s16typed_throws_abi16callImplAsync_h5ySi_S4itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr %call.aggresult, ptr {{%.*}}, i1 %2, ptr %swifterror)
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, ptr } [[CALL_RES]], 1
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror1, align 8
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror1, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror.x = getelementptr inbounds{{.*}} %T16typed_throws_abi8TwoWordsV, ptr %swifterror, i32 0, i32 0
// CHECK:   %swifterror.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror.x, i32 0, i32 0
// CHECK:   [[ERROR_X:%.*]] = load i64, ptr %swifterror.x._value, align 8
// CHECK:   [[ERROR_Y:%.*]] = load i64, ptr %swifterror.y._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr %.elt{{.*}}, align 8
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}})
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[ERROR_X]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[ERROR_Y]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_h5(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int, Int) {
    do {
        return try await impl.h5(b)
    } catch {
        return (error.x, error.y, 0, 0, 0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i0ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_i0ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i0(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        try await impl.i0(b)
        return (0, 0, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i1ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_i1ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i1(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        return try await (impl.i1(b), 0, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i2ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_i2ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ 0, %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i2(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        let res = try await impl.i2(b)
        return (res.0, res.1, 0)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i3ySi_S2itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_i3ySi_S2itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, ptr } [[CALL_RES]], 4
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i3(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int) {
    do {
        return try await impl.i3(b)
    } catch {
        return (error.x, error.y, error.z)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i4ySi_S3itAA0eF0V_SbtYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 0, ptr @"$s16typed_throws_abi16callImplAsync_i4ySi_S3itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, i64, i64, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64i64i64p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr @"{{.*}}", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 2
// CHECK:   [[CALL_RES3:%.*]] = extractvalue { i64, i64, i64, i64 } {{%.*}}, 3
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, i64, i64, i64, i64, ptr } [[CALL_RES]], 5
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], i64 [[RETVAL1]], i64 [[RETVAL2]], i64 [[RETVAL3]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES0]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i4(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int) {
    do {
        return try await impl.i4(b)
    } catch {
        return (error.x, error.y, error.z, 0)
    }
}


// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi16callImplAsync_i5ySi_S4itAA0eF0V_SbtYaF"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2)
// CHECK:   %swifterror = alloca %T16typed_throws_abi10ThreeWordsV, align 8
// CHECK:   %call.aggresult = alloca <{ %TSi, %TSi, %TSi, %TSi, %TSi }>, align 8
// CHECK:   %swifterror1 = alloca swifterror ptr, align 8
// CHECK:   [[CALL:%.*]] = call token @llvm.coro.id.async(i32 16, i32 16, i32 1, ptr @"$s16typed_throws_abi16callImplAsync_i5ySi_S4itAA0eF0V_SbtYaFTu")
// CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token [[CALL]], ptr null)
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   [[CORO_RESUME:%.*]] = call ptr @llvm.coro.async.resume()
// CHECK:   [[CALL_RES:%.*]] = call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s(i32 {{[0-9]+}}, ptr [[CORO_RESUME]], ptr @__swift_async_resume_project_context, ptr @"{{.*}}", ptr %call.aggresult, ptr {{%.*}}, i1 %2, ptr %swifterror)
// CHECK:   [[ERROR_FLAG:%.*]] = extractvalue { ptr, ptr } [[CALL_RES]], 1
// CHECK:   store ptr [[ERROR_FLAG]], ptr %swifterror1, align 8
// CHECK:   [[CALL_RES0:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES1:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES2:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES3:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[CALL_RES4:%.*]] = load i64, ptr %call.aggresult.elt{{.*}}, align 8
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror1, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   %swifterror.x = getelementptr inbounds{{.*}} %T16typed_throws_abi10ThreeWordsV, ptr %swifterror, i32 0, i32 0
// CHECK:   %swifterror.x._value = getelementptr inbounds{{.*}} %TSi, ptr %swifterror.x, i32 0, i32 0
// CHECK:   [[ERROR_X:%.*]] = load i64, ptr %swifterror.x._value, align 8
// CHECK:   [[ERROR_Y:%.*]] = load i64, ptr %swifterror.y._value, align 8
// CHECK:   [[ERROR_Z:%.*]] = load i64, ptr %swifterror.z._value, align 8
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi i64 [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i64 [ [[CALL_RES1]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi i64 [ [[CALL_RES2]], %entry ]
// CHECK:   [[SUCCESS_RES3:%.*]] = phi i64 [ [[CALL_RES3]], %entry ]
// CHECK:   [[SUCCESS_RES4:%.*]] = phi i64 [ [[CALL_RES4]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi i64 [ [[ERROR_RES1:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i64 [ [[ERROR_RES2:%.*]], %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES3]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = phi i64 [ 0, %[[SET_ERROR]] ], [ [[SUCCESS_RES4]], %[[SUCCESS]] ]
// CHECK:   store i64 [[RETVAL0]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL1]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL2]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL3]], ptr %.elt{{.*}}, align 8
// CHECK:   store i64 [[RETVAL4]], ptr %.elt{{.*}}, align 8
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}})
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[ERROR_X]], %typed.error.load ]
// CHECK:   [[ERROR_RES1]] = phi i64 [ [[ERROR_Y]], %typed.error.load ]
// CHECK:   [[ERROR_RES2]] = phi i64 [ [[ERROR_Z]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror1, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callImplAsync_i5(_ impl: ImplAsync, _ b: Bool) async -> (Int, Int, Int, Int, Int) {
    do {
        return try await impl.i5(b)
    } catch {
        return (error.x, error.y, error.z, 0, 0)
    }
}

// CHECK: define hidden swiftcc { float, float, i64 } @"$s16typed_throws_abi14nonMatching_f0ySf_SftSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
// CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   ret { float, float, i64 } { float 1.000000e+00, float 2.000000e+00, i64 undef }
// CHECK: [[FAIL]]:
// CHECK:   [[ERROR_RES0:%.*]] = call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
// CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
// CHECK:   [[ERROR_RES:%.*]] = insertvalue { float, float, i64 } undef, i64 [[ERROR_RES0]], 2
// CHECK:   ret { float, float, i64 } [[ERROR_RES]]
// CHECK: }
func nonMatching_f0(_ b: Bool) throws(OneWord) -> (Float, Float) {
    guard b else {
        throw OneWord()
    }
    return (1.0, 2.0)
}

// CHECK: define hidden swiftcc { i64, float, float } @"$s16typed_throws_abi18callNonMatching_f0ySi_S2ftSbF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { float, float, i64 } @"$s16typed_throws_abi14nonMatching_f0ySf_SftSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { float, float, i64 } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { float, float, i64 } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { float, float, i64 } [[CALL_RES]], 2
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi float [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi float [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 1, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = insertvalue { i64, float, float } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, float, float } [[RETVAL3]], float [[RETVAL1]], 1
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, float, float } [[RETVAL4]], float [[RETVAL2]], 2
// CHECK:   ret { i64, float, float } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callNonMatching_f0(_ b: Bool) -> (Int, Float, Float) {
    do {
        let res = try nonMatching_f0(b)
        return (1, res.0, res.1)
    } catch {
        return (error.x, 0.0, 0.0)
    }
}

// define hidden swiftcc { float, i64, float } @"$s16typed_throws_abi14nonMatching_f1ySf_SbSftSbAA7OneWordVYKF"(i1 %0, ptr swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2)
// CHECK:   br i1 %0, label %[[SUCCESS:.*]], label %[[FAIL:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   ret { float, i64, float } { float 1.000000e+00, i64 1, float 2.000000e+00 }
// CHECK: [[FAIL]]:
// CHECK:   [[ERROR_RES0:%.*]] = call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
// CHECK:   store ptr inttoptr (i64 1 to ptr), ptr %2, align 8
// CHECK:   [[ERROR_RES:%.*]] = insertvalue { float, i64, float } undef, i64 [[ERROR_RES0]], 1
// CHECK:   ret { float, i64, float } [[ERROR_RES]]
// }
func nonMatching_f1(_ b: Bool) throws(OneWord) -> (Float, Bool, Float) {
    guard b else {
        throw OneWord()
    }
    return (1.0, true, 2.0)
}

// CHECK: define hidden swiftcc { i64, float, i1, float } @"$s16typed_throws_abi18callNonMatching_f1ySi_SfSbSftSbF"(i1 %0)
// CHECK:   %swifterror = alloca swifterror ptr, align 8
// CHECK:   [[CALL_RES:%.*]] = call swiftcc { float, i64, float } @"$s16typed_throws_abi14nonMatching_f1ySf_SbSftSbAA7OneWordVYKF"(i1 %0, ptr swiftself undef, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %swifterror)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { float, i64, float } [[CALL_RES]], 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { float, i64, float } [[CALL_RES]], 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { float, i64, float } [[CALL_RES]], 2
// CHECK:   [[CALL_RES1_TRUNC:%.*]] = trunc i64 [[CALL_RES1]] to i1
// CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi float [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i1 [ [[CALL_RES1_TRUNC]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi float [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 1, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i1 [ false, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL4:%.*]] = insertvalue { i64, float, i1, float } undef, i64 [[RETVAL0]], 0
// CHECK:   [[RETVAL5:%.*]] = insertvalue { i64, float, i1, float } [[RETVAL4]], float [[RETVAL1]], 1
// CHECK:   [[RETVAL6:%.*]] = insertvalue { i64, float, i1, float } [[RETVAL5]], i1 [[RETVAL2]], 2
// CHECK:   [[RETVAL:%.*]] = insertvalue { i64, float, i1, float } [[RETVAL6]], float [[RETVAL3]], 3
// CHECK:   ret { i64, float, i1, float } [[RETVAL]]
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
func callNonMatching_f1(_ b: Bool) -> (Int, Float, Bool, Float) {
    do {
        let res = try nonMatching_f1(b)
        return (1, res.0, res.1, res.2)
    } catch {
        return (error.x, 0.0, false, 0.0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi20nonMatching_f0_asyncySf_SftSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
// CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi20nonMatching_f0_asyncySf_SftSbYaAA7OneWordVYKF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, float 1.000000e+00, float 2.000000e+00, i64 undef, ptr null)
// CHECK:   unreachable
// CHECK: 18:
// CHECK:   [[ERROR_X:%.*]] = call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
// CHECK:   [[ERROR_RET:%.*]] = insertvalue { float, float, i64 } undef, i64 [[ERROR_X]], 2
// CHECK:   [[ERROR_RET0:%.*]] = extractvalue { float, float, i64 } [[ERROR_RET]], 0
// CHECK:   [[ERROR_RET1:%.*]] = extractvalue { float, float, i64 } [[ERROR_RET]], 1
// CHECK:   [[ERROR_RET2:%.*]] = extractvalue { float, float, i64 } [[ERROR_RET]], 2
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi20nonMatching_f0_asyncySf_SftSbYaAA7OneWordVYKF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, float [[ERROR_RET0]], float [[ERROR_RET1]], i64 [[ERROR_RET2]], ptr inttoptr (i64 1 to ptr))
// CHECK:   unreachable
// CHECK: }
@available(SwiftStdlib 6.0, *)
func nonMatching_f0_async(_ b: Bool) async throws(OneWord) -> (Float, Float) {
    guard b else {
        throw OneWord()
    }
    return (1.0, 2.0)
}


// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi24callNonMatching_f0_asyncySi_S2ftSbYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   call { ptr, float, float, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0f32f32i64p0s(i32 1024, ptr {{%.*}}, ptr @__swift_async_resume_project_context, ptr @"$s16typed_throws_abi24callNonMatching_f0_asyncySi_S2ftSbYaF.0", ptr @"$s16typed_throws_abi20nonMatching_f0_asyncySf_SftSbYaAA7OneWordVYKF", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { float, float, i64 } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { float, float, i64 } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { float, float, i64 } {{%.*}}, 2
// CHECK:   [[ERROR:%.*]] = extractvalue { ptr, float, float, i64, ptr } {{%.*}}, 4
// CHECK:   store ptr [[ERROR]], ptr %swifterror, align 8
// CHECK:   [[ERROR0:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR0]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi float [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi float [ [[CALL_RES1]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 1, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi24callNonMatching_f0_asyncySi_S2ftSbYaF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], float [[RETVAL1]], float [[RETVAL2]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES2]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callNonMatching_f0_async(_ b: Bool) async -> (Int, Float, Float) {
    do {
        let res = try await nonMatching_f0_async(b)
        return (1, res.0, res.1)
    } catch {
        return (error.x, 0.0, 0.0)
    }
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi20nonMatching_f1_asyncySf_SbSftSbYaAA7OneWordVYKF"(ptr swiftasync %0, i1 %1)
// CHECK:   br i1 %1, label %[[SUCCESS:.*]], label %[[ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi20nonMatching_f1_asyncySf_SbSftSbYaAA7OneWordVYKF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, float 1.000000e+00, i64 1, float 2.000000e+00, ptr null)
// CHECK:   unreachable
// CHECK: [[ERROR]]:
// CHECK:   [[ERROR_X:%.*]] = call swiftcc i64 @"$s16typed_throws_abi7OneWordVACycfC"()
// CHECK:   [[ERROR_RET:%.*]] = insertvalue { float, i64, float } undef, i64 [[ERROR_X]], 1
// CHECK:   [[ERROR_RET0:%.*]] = extractvalue { float, i64, float } [[ERROR_RET]], 0
// CHECK:   [[ERROR_RET1:%.*]] = extractvalue { float, i64, float } [[ERROR_RET]], 1
// CHECK:   [[ERROR_RET2:%.*]] = extractvalue { float, i64, float } [[ERROR_RET]], 2
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi20nonMatching_f1_asyncySf_SbSftSbYaAA7OneWordVYKF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, float [[ERROR_RET0]], i64 [[ERROR_RET1]], float [[ERROR_RET2]], ptr inttoptr (i64 1 to ptr))
// CHECK:   unreachable
// CHECK: }
@available(SwiftStdlib 6.0, *)
func nonMatching_f1_async(_ b: Bool) async throws(OneWord) -> (Float, Bool, Float) {
    guard b else {
        throw OneWord()
    }
    return (1.0, true, 2.0)
}

// CHECK: define hidden swifttailcc void @"$s16typed_throws_abi24callNonMatching_f1_asyncySi_SfSbSftSbYaF"(ptr swiftasync %0, i1 %1)
// CHECK:   call { ptr, float, i64, float, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0f32i64f32p0s(i32 1024, ptr {{%.*}}, ptr @__swift_async_resume_project_context, ptr @"$s16typed_throws_abi24callNonMatching_f1_asyncySi_SfSbSftSbYaF.0", ptr @"$s16typed_throws_abi20nonMatching_f1_asyncySf_SbSftSbYaAA7OneWordVYKF", ptr {{%.*}}, i1 %1)
// CHECK:   [[CALL_RES0:%.*]] = extractvalue { float, i64, float } {{%.*}}, 0
// CHECK:   [[CALL_RES1:%.*]] = extractvalue { float, i64, float } {{%.*}}, 1
// CHECK:   [[CALL_RES2:%.*]] = extractvalue { float, i64, float } {{%.*}}, 2
// CHECK:   [[CALL_RES1_TRUNC:%.*]] = trunc i64 [[CALL_RES1]] to i1
// CHECK:   [[ERROR:%.*]] = extractvalue { ptr, float, i64, float, ptr } {{%.*}}, 4
// CHECK:   store ptr [[ERROR]], ptr %swifterror, align 8
// CHECK:   [[ERROR0:%.*]] = load ptr, ptr %swifterror, align 8
// CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR0]], null
// CHECK:   br i1 [[ISERROR]], label %typed.error.load, label %[[SUCCESS:.*]]
// CHECK: typed.error.load:
// CHECK:   br label %[[SET_ERROR:.*]]
// CHECK: [[SUCCESS]]:
// CHECK:   [[SUCCESS_RES0:%.*]] = phi float [ [[CALL_RES0]], %entry ]
// CHECK:   [[SUCCESS_RES1:%.*]] = phi i1 [ [[CALL_RES1_TRUNC]], %entry ]
// CHECK:   [[SUCCESS_RES2:%.*]] = phi float [ [[CALL_RES2]], %entry ]
// CHECK:   br label %[[COMMON_RET:.*]]
// CHECK: [[COMMON_RET]]:
// CHECK:   [[RETVAL0:%.*]] = phi i64 [ [[ERROR_RES0:%.*]], %[[SET_ERROR]] ], [ 1, %[[SUCCESS]] ]
// CHECK:   [[RETVAL1:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES0]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL2:%.*]] = phi i1 [ false, %[[SET_ERROR]] ], [ [[SUCCESS_RES1]], %[[SUCCESS]] ]
// CHECK:   [[RETVAL3:%.*]] = phi float [ 0.000000e+00, %[[SET_ERROR]] ], [ [[SUCCESS_RES2]], %[[SUCCESS]] ]
// CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr {{%.*}}, i1 false, ptr @"$s16typed_throws_abi24callNonMatching_f1_asyncySi_SfSbSftSbYaF{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 [[RETVAL0]], float [[RETVAL1]], i1 [[RETVAL2]], float [[RETVAL3]])
// CHECK:   unreachable
// CHECK: [[SET_ERROR]]:
// CHECK:   [[ERROR_RES0]] = phi i64 [ [[CALL_RES1]], %typed.error.load ]
// CHECK:   store ptr null, ptr %swifterror, align 8
// CHECK:   br label %[[COMMON_RET]]
// CHECK: }
@available(SwiftStdlib 6.0, *)
func callNonMatching_f1_async(_ b: Bool) async -> (Int, Float, Bool, Float) {
    do {
        let res = try await nonMatching_f1_async(b)
        return (1, res.0, res.1, res.2)
    } catch {
        return (error.x, 0.0, false, 0.0)
    }
}

protocol P {
    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f0yySbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret void
    // CHECK: success:
    // CHECK:   ret void
    // CHECK: }
    func f0(_ b: Bool) throws(Empty)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2f1ySiSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 undef
    // CHECK: success:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func f1(_ b: Bool) throws(Empty) -> Int

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2f2ySi_SitSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func f2(_ b: Bool) throws(Empty) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2f3ySi_S2itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2f4ySi_S3itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } undef
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f5ySi_S4itSbAA5EmptyVYKFTj"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2g0yySbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: success:
    // CHECK:   ret i64 undef
    // CHECK: }
    func g0(_ b: Bool) throws(OneWord)

    // CHECK: define hidden swiftcc i64 @"$s16typed_throws_abi1PP2g1ySiSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: success:
    // CHECK:   ret i64 {{%.*}}
    // CHECK: }
    func g1(_ b: Bool) throws(OneWord) -> Int

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2g2ySi_SitSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2g3ySi_S2itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2g4ySi_S3itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2g5ySi_S4itSbAA7OneWordVYKFTj"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int)

    // CHECK:  define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2h0yySbAA8TwoWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64 } undef
    // CHECK: }
    func h0(_ b: Bool) throws(TwoWords)

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2h1ySiSbAA8TwoWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func h1(_ b: Bool) throws(TwoWords) -> Int

    // CHECK: define hidden swiftcc { i64, i64 } @"$s16typed_throws_abi1PP2h2ySi_SitSbAA8TwoWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64 } {{%.*}}
    // CHECK: }
    func h2(_ b: Bool) throws(TwoWords) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2h3ySi_S2itSbAA8TwoWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func h3(_ b: Bool) throws(TwoWords) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2h4ySi_S3itSbAA8TwoWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func h4(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2h5ySi_S4itSbAA8TwoWordsVYKFTj"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func h5(_ b: Bool) throws(TwoWords) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2i0yySbAA10ThreeWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } undef
    // CHECK: }
    func i0(_ b: Bool) throws(ThreeWords)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2i1ySiSbAA10ThreeWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i1(_ b: Bool) throws(ThreeWords) -> Int

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2i2ySi_SitSbAA10ThreeWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i2(_ b: Bool) throws(ThreeWords) -> (Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64 } @"$s16typed_throws_abi1PP2i3ySi_S2itSbAA10ThreeWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i3(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { i64, i64, i64, i64 } @"$s16typed_throws_abi1PP2i4ySi_S3itSbAA10ThreeWordsVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { i64, i64, i64, i64 } {{%.*}}
    // CHECK: }
    func i4(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2i5ySi_S4itSbAA10ThreeWordsVYKFTj"(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias{{( nocapture)?}} sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>){{( captures\(none\))?}} %0, i1 %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func i5(_ b: Bool) throws(ThreeWords) -> (Int, Int, Int, Int, Int)
}

@available(SwiftStdlib 6.0, *)
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

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2f5ySi_S4itSbYaAA5EmptyVYKFTj"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func f5(_ b: Bool) async throws(Empty) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g0yySbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr [[ERROR]])
    // CHECK: }
    func g0(_ b: Bool) async throws(OneWord)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g1ySiSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g1(_ b: Bool) async throws(OneWord) -> Int

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g2ySi_SitSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g2(_ b: Bool) async throws(OneWord) -> (Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g3ySi_S2itSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}} ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g3(_ b: Bool) async throws(OneWord) -> (Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g4ySi_S3itSbYaAA7OneWordVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g4(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g5ySi_S4itSbYaAA7OneWordVYKFTj"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h0yySbYaAA8TwoWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, ptr [[ERROR]])
    // CHECK: }
    func h0(_ b: Bool) async throws(TwoWords)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h1ySiSbYaAA8TwoWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func h1(_ b: Bool) async throws(TwoWords) -> Int

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h2ySi_SitSbYaAA8TwoWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func h2(_ b: Bool) async throws(TwoWords) -> (Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h3ySi_S2itSbYaAA8TwoWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}} ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func h3(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h4ySi_S3itSbYaAA8TwoWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func h4(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2h5ySi_S4itSbYaAA8TwoWordsVYKFTj"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func h5(_ b: Bool) async throws(TwoWords) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i0yySbYaAA10ThreeWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 undef, i64 undef, i64 undef, ptr [[ERROR]])
    // CHECK: }
    func i0(_ b: Bool) async throws(ThreeWords)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i1ySiSbYaAA10ThreeWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func i1(_ b: Bool) async throws(ThreeWords) -> Int

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i2ySi_SitSbYaAA10ThreeWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func i2(_ b: Bool) async throws(ThreeWords) -> (Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i3ySi_S2itSbYaAA10ThreeWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func i3(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i4ySi_S3itSbYaAA10ThreeWordsVYKFTj"(ptr swiftasync %0, i1 %1, ptr noalias swiftself %2, ptr %3, ptr %4)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: success:
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, i64 {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func i4(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int)

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2i5ySi_S4itSbYaAA10ThreeWordsVYKFTj"(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func i5(_ b: Bool) async throws(ThreeWords) -> (Int, Int, Int, Int, Int)
}
