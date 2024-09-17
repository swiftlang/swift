// RUN: %target-swift-frontend -primary-file %s -emit-irgen -enable-library-evolution | %FileCheck %s

struct Empty: Error {}

struct OneWord: Error {
    let x = 1
}

protocol P {
    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f0yySbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret void
    // CHECK: success:
    // CHECK:   ret void
    // CHECK: }
    func f0(_ b: Bool) throws(Empty)

    // CHECK: define hidden swiftcc {{i64|i32}} @"$s16typed_throws_abi1PP2f1ySiSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret {{i64|i32}} undef
    // CHECK: success:
    // CHECK:   ret {{i64|i32}} {{%.*}}
    // CHECK: }
    func f1(_ b: Bool) throws(Empty) -> Int

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2f2ySi_SitSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}} } undef
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func f2(_ b: Bool) throws(Empty) -> (Int, Int)

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2f3ySi_S2itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } undef
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func f3(_ b: Bool) throws(Empty) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2f4ySi_S3itSbAA5EmptyVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } undef
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func f4(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2f5ySi_S4itSbAA5EmptyVYKFTj"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   call swiftcc void {{%.*}}(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    // CHECK:   ret void
    // CHECK: }
    func f5(_ b: Bool) throws(Empty) -> (Int, Int, Int, Int, Int)

    // CHECK: define hidden swiftcc {{i64|i32}} @"$s16typed_throws_abi1PP2g0yySbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret {{i64|i32}} {{%.*}}
    // CHECK: success:
    // CHECK:   ret {{i64|i32}} undef
    // CHECK: }
    func g0(_ b: Bool) throws(OneWord)

    // CHECK: define hidden swiftcc {{i64|i32}} @"$s16typed_throws_abi1PP2g1ySiSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret {{i64|i32}} {{%.*}}
    // CHECK: success:
    // CHECK:   ret {{i64|i32}} {{%.*}}
    // CHECK: }
    func g1(_ b: Bool) throws(OneWord) -> Int

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2g2ySi_SitSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func g2(_ b: Bool) throws(OneWord) -> (Int, Int)

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2g3ySi_S2itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func g3(_ b: Bool) throws(OneWord) -> (Int, Int, Int)

    // CHECK: define hidden swiftcc { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } @"$s16typed_throws_abi1PP2g4ySi_S3itSbAA7OneWordVYKFTj"(i1 %0, ptr noalias swiftself %1, ptr noalias nocapture swifterror dereferenceable(8) %2, ptr %3, ptr %4)
    // CHECK:   [[ERROR:%.*]] = load ptr, ptr %2, align 8
    // CHECK:   [[ISERROR:%.*]] = icmp ne ptr [[ERROR]], null
    // CHECK:   br i1 [[ISERROR]], label %failure, label %success
    // CHECK: failure:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: success:
    // CHECK:   ret { {{i64|i32}}, {{i64|i32}}, {{i64|i32}}, {{i64|i32}} } {{%.*}}
    // CHECK: }
    func g4(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int)

    // CHECK: define hidden swiftcc void @"$s16typed_throws_abi1PP2g5ySi_S4itSbAA7OneWordVYKFTj"(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    //   call swiftcc void {{%.*}}(ptr noalias nocapture sret(<{ %TSi, %TSi, %TSi, %TSi, %TSi }>) %0, i1 %1, ptr noalias swiftself %2, ptr noalias nocapture swifterror dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
    //   ret void
    // }
    func g5(_ b: Bool) throws(OneWord) -> (Int, Int, Int, Int, Int)
}
