// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/Mutex.swift
// RUN: %target-swift-frontend -enable-experimental-feature RawLayout -emit-ir -target %target-swift-6.0-abi-triple -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -module-name stdlib %t/Mutex.swift | %FileCheck %t/Mutex.swift --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

// REQUIRES: synchronization
// REQUIRES: swift_feature_RawLayout

import Synchronization

struct GenericMutex<T>: ~Copyable {
  let mutex: Mutex<T> // this previously crashed the compiler
}

// Force emission of GenericMutex...
// CHECK: %T6stdlib12GenericMutexVyytG
func forceGenericMutex() -> GenericMutex<Void> {
  GenericMutex(mutex: Mutex(()))
}

// Mutex<Void.Type> must reserve pointer-sized storage for its value. It used
// to reserve none, so storing the value overwrote the lock. Check the size of
// its storage: the existence of [n x i8] indicates a non-zero size.
// CHECK: %T15Synchronization5_CellVyytmG = type <{ [{{[1-9][0-9]*}} x i8] }>
public struct VoidMetatypeMutex: ~Copyable {
  public let mutex: Mutex<Void.Type>
}

final class Awaitable<Value, Failure>: Sendable where Value: Sendable, Failure: Error {
  struct State {
    var pendingConsumers: [CheckedContinuation<Value, Failure>] = []
    var result: Result<Value, Failure>?
  }

  let state: Mutex<State> // This previously crashed the compiler...

  init() {
    self.state = Mutex(.init())
  }
}

// CHECK: define {{.*}} ptr @"$s15Synchronization5MutexVy6stdlib9AwaitableC5StateVyxq__GGs8SendableRzs5ErrorR_r0_lWOb"(ptr [[SRC:%.*]], ptr [[DEST:%.*]], ptr {{%.*}}, ptr {{%.*}}, ptr {{%.*}}, ptr {{%.*}}, ptr {{%.*}}, ptr {{%.*}}, ptr [[MUTEX:%.*]])
// CHECK:   [[DEST_HANDLE_PTR:%.*]] = getelementptr inbounds{{.*}} %T15Synchronization5MutexV{{.*}}, ptr [[DEST]], i32 0, i32 0
// CHECK:   [[SRC_HANDLE_PTR:%.*]] = getelementptr inbounds{{.*}} %T15Synchronization5MutexV{{.*}}, ptr [[SRC]], i32 0, i32 0
// CHECK:   call void @llvm.memcpy.p0.p0.i{{32|64}}(ptr {{.*}} [[DEST_HANDLE_PTR]], ptr {{.*}} [[SRC_HANDLE_PTR]], i{{32|64}} {{.*}}, i1 false)
// CHECK:   [[DEST_MUTEX_VALUE_OFFSET_PTR:%.*]] = getelementptr inbounds i32, ptr [[MUTEX]], i{{32 4|64 7}}
// CHECK:   [[DEST_MUTEX_VALUE_OFFSET:%.*]] = load i32, ptr [[DEST_MUTEX_VALUE_OFFSET_PTR]]
// CHECK:   [[DEST_VALUE_PTR:%.*]] = getelementptr inbounds i8, ptr [[DEST]], i32 [[DEST_MUTEX_VALUE_OFFSET]]
// CHECK:   [[SRC_MUTEX_VALUE_OFFSET_PTR:%.*]] = getelementptr inbounds i32, ptr [[MUTEX]], i{{32 4|64 7}}
// CHECK:   [[SRC_MUTEX_VALUE_OFFSET:%.*]] = load i32, ptr [[SRC_MUTEX_VALUE_OFFSET_PTR]]
// CHECK:   [[SRC_VALUE_PTR:%.*]] = getelementptr inbounds i8, ptr [[SRC]], i32 [[SRC_MUTEX_VALUE_OFFSET]]

// These GEPs used to cause compiler crashes because they were incorrectly typed...
// CHECK:   [[DEST_PENDING_CONSUMERS_PTR:%.*]] = getelementptr inbounds{{.*}} %T6stdlib9AwaitableC5StateV{{.*}}, ptr [[DEST_VALUE_PTR]], i32 0, i32 0
// CHECK:   [[SRC_PENDING_CONSUMERS_PTR:%.*]] = getelementptr inbounds{{.*}} %T6stdlib9AwaitableC5StateV{{.*}}, ptr [[SRC_VALUE_PTR]], i32 0, i32 0
