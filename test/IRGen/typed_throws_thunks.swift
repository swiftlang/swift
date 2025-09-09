// RUN: %target-swift-frontend -primary-file %s -emit-irgen -target %target-swift-5.1-abi-triple -runtime-compatibility-version none -target %module-target-future -enable-library-evolution | %FileCheck %s --check-prefix=CHECK

// REQUIRES: PTRSIZE=64
// UNSUPPORTED: CPU=arm64e

@frozen
public struct FixedSize : Error {
   var x = 0
   var y = 0
}

public protocol P {
  associatedtype Failure: Error = any Error

  func f(body: () throws -> Void) async rethrows
  func f2(body: () throws -> Void) rethrows

  func g(body: () throws(Failure) -> Void) async throws(Failure)
  func g4(body: () throws(FixedSize) -> Void) async throws(FixedSize)

  func g2(body: () throws(Failure) -> Void) throws(Failure)
  func g3(body: () throws(FixedSize) -> Void) throws(FixedSize)
}

extension P {
 // CHECK-LABEL: define{{.*}} swifttailcc void @"$s19typed_throws_thunks1PP1g4bodyyyy7FailureQzYKXE_tYaAGYKFTj"(ptr swiftasync %0, ptr %1, ptr %2, ptr noalias swiftself %3, ptr %4, ptr %5, ptr %6)
 // CHECK-NOT: ret
 // CHECK:  call { ptr, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0p0s({{.*}}, ptr %1, ptr %2, ptr %3, ptr %4, ptr %5, ptr %6)
  public func g(body: () throws(Failure) -> Void) async throws(Failure) {
    do {
      return try await f(body: body)
    } catch {
      throw error as! Failure
    }
  }

  // CHECK-LABEL: define{{.*}} swifttailcc void @"$s19typed_throws_thunks1PP2g44bodyyyyAA9FixedSizeVYKXE_tYaAGYKFTj"(ptr swiftasync %0, ptr %1, ptr %2, ptr noalias swiftself %3, ptr %4, ptr %5)
	// CHECK-NOT: ret
	// CHECK: call { ptr, i64, i64, ptr } (i32, ptr, ptr, ...) @llvm.coro.suspend.async.sl_p0i64i64p0s({{.*}} ptr %1, ptr %2, ptr %3, ptr %4, ptr %5)
  public func g4(body: () throws(FixedSize) -> Void) async throws(FixedSize) {
    do {
      return try await f(body: body)
    } catch {
      throw error as! FixedSize
    }
  }

  // CHECK-LABEL: define{{.*}} swiftcc void @"$s19typed_throws_thunks1PP2g24bodyyyy7FailureQzYKXE_tAGYKFTj"(ptr %0, ptr %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)
	// CHECK-NOT: ret
  // CHECK: call swiftcc void {{.*}}(ptr %0, ptr %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5, ptr %6)

  public func g2(body: () throws(Failure) -> Void) throws(Failure) {
    do {
      return try f2(body: body)
    } catch {
      throw error as! Failure
    }
  }

  // CHECK-LABEL: define{{.*}} swiftcc { i64, i64 } @"$s19typed_throws_thunks1PP2g34bodyyyyAA9FixedSizeVYKXE_tAGYKFTj"(ptr %0, ptr %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5)
  // CHECK-NOT: ret
  // CHECK:  call swiftcc { i64, i64 } {{.*}}(ptr %0, ptr %1, ptr noalias swiftself %2, ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %3, ptr %4, ptr %5)


  public func g3(body: () throws(FixedSize) -> Void) throws(FixedSize) {
  }
}

protocol P2 {
// CHECK-LABEL: define{{.*}} swiftcc void @"$s19typed_throws_thunks2P2P1fyyAA1EOYKFTj"(
// CHECK-SAME:      ptr noalias swiftself %0, 
// CHECK-SAME:      ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %1, 
// CHECK-SAME:      ptr %2, ptr %3
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       failure:
// CHECK-NOT:     ret void undef
// CHECK:         ret void
// CHECK:       success:
// CHECK-NEXT:    ret void
    func f() throws(E)
// CHECK-LABEL: define{{.*}} swiftcc i8 @"$s19typed_throws_thunks2P2P1gyys4Int8VYKFTj"(
// CHECk-SAME:      ptr noalias swiftself %0
// CHECK-SAME:      ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %1
// CHECK-SAME:      ptr %2
// CHECK-SAME:      ptr %3
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       failure:
// CHECK-NEXT:    ret i8 %{{.*}}
// CHECK:       success:
// CHECK-NEXT:    ret i8 undef
    func g() throws(Int8)
// CHECK-LABEL: define{{.*}} swiftcc i8 @"$s19typed_throws_thunks2P2P1hs4Int8VyAA1EOYKFTj"(
// CHECK-SAME:      ptr noalias swiftself %0
// CHECK-SAME:      ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %1
// CHECK-SAME:      ptr %2 
// CHECK-SAME:      ptr %3
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       failure:
// CHECK-NEXT:    ret i8 undef
// CHECK:       success:
// CHECK-NEXT:    ret i8 %{{.*}}
    func h() throws(E) -> Int8
// CHECK-LABEL: define{{.*}} swiftcc i8 @"$s19typed_throws_thunks2P2P1is4Int8VyAFYKFTj"(
// CHECK-SAME:      ptr noalias swiftself %0
// CHECK-SAME:      ptr noalias{{( nocapture)?}} swifterror{{( captures\(none\))?}} dereferenceable(8) %1
// CHECK-SAME:      ptr %2
// CHECK-SAME:      ptr %3
// CHECK-SAME:  )
// CHECK-SAME:  {
// CHECK:       failure:
// CHECK-NEXT:    ret i8 %{{.*}}
// CHECK:       success:
// CHECK-NEXT:    ret i8 %{{.*}}
    func i() throws(Int8) -> Int8
}

extension Int8 : Error {}

enum E: Error {
    case e
}
