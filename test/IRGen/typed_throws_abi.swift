// RUN: %target-swift-frontend -primary-file %s -emit-irgen -enable-library-evolution | %FileCheck %s

// REQUIRES: PTRSIZE=64

struct Empty: Error {}

struct OneWord: Error {
    let x = 1
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

    // CHECK: define hidden swifttailcc void @"$s16typed_throws_abi6PAsyncP2g5ySi_S4itSbYaAA7OneWordVYKFTj"(ptr noalias nocapture %0, ptr swiftasync %1, i1 %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   %swifterror = alloca swifterror ptr
    // CHECK:   [[CORO:%.*]] = call ptr @llvm.coro.begin(token {{%.*}}, ptr null)
    // CHECK:   store ptr null, ptr %swifterror
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %swifterror
    // CHECK:   call i1 (ptr, i1, ...) @llvm.coro.end.async(ptr [[CORO]], i1 false, ptr @"{{.*}}", ptr {{%.*}}, ptr {{%.*}}, ptr [[ERROR]])
    // CHECK: }
    func g5(_ b: Bool) async throws(OneWord) -> (Int, Int, Int, Int, Int)
}
