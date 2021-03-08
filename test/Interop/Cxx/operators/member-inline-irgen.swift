// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s
//
// We can't yet call member functions correctly on Windows (SR-13129).
// XFAIL: OS=windows-msvc

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: call [[RES:i32|i64]] [[NAME:@(_ZN18LoadableIntWrappermiES_|"\?\?GLoadableIntWrapper@@QEAA\?AU0@U0@@Z")]](%struct.LoadableIntWrapper* {{%[0-9]+}}, {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* byval\(.*\) align 4}} {{%[0-9]+}})
// CHECK: define linkonce_odr [[RES]] [[NAME]](%struct.LoadableIntWrapper* nonnull dereferenceable(4) {{.*}}, {{i32 %rhs.coerce|\[1 x i32\] %rhs.coerce|i64 %rhs.coerce|%struct.LoadableIntWrapper\* byval\(%struct.LoadableIntWrapper\) align 4 %rhs}})

public func call(_ wrapper: inout LoadableIntWrapper, _ arg: Int32) -> Int32 { wrapper(arg) }

// CHECK: call [[RES:i32|i64]] [[NAME:@(_ZN18LoadableIntWrapperclEi|"\?\?GLoadableIntWrapper@@QEAAHH@Z")]](%struct.LoadableIntWrapper* {{%[0-9]+}}, {{i32|\[1 x i32\]|i64|%struct.LoadableIntWrapper\* byval\(.*\) align 4}} {{%[0-9]+}})
// CHECK: define linkonce_odr [[RES]] [[NAME]](%struct.LoadableIntWrapper* nonnull dereferenceable(4) {{.*}}, {{i32 %x|\[1 x i32\] %x|i64 %x|%struct.LoadableIntWrapper\* byval\(%struct.LoadableIntWrapper\) align 4 %rhs}})

public func call(_ wrapper: inout AddressOnlyIntWrapper) -> Int32 { wrapper() }

// CHECK: call [[RES:i32|i64]] [[NAME:@(_ZN21AddressOnlyIntWrapperclEv|"\?\?GAddressOnlyIntWrapper@@QEAAHXZ")]](%struct.AddressOnlyIntWrapper* {{%[0-9]+}})
// CHECK: define linkonce_odr [[RES]] [[NAME]](%struct.AddressOnlyIntWrapper* nonnull dereferenceable(4) {{.*}})

public func index(_ arr: inout ReadOnlyIntArray, _ arg: Int32) -> Int32 { arr[arg] }

// CHECK: call [[RES:i32|i64]]* [[NAME:@(_ZNK16ReadOnlyIntArrayixEi|"\?\?AReadOnlyIntArray@@QEBAAEBHH@Z")]](%struct.ReadOnlyIntArray* {{%[0-9]+}}, {{i32|i64}} {{%[0-9]+}})
// CHECK: define linkonce_odr nonnull align 4 dereferenceable(4) [[RES]]* [[NAME]](%struct.ReadOnlyIntArray* nonnull dereferenceable(20) {{.*}}, {{i32 %x|\[1 x i32\] %x|i64 %x|%struct.ReadOnlyIntArray\* byval\(%struct.ReadOnlyIntArray\) align 2 %rhs}})
// CHECK:   [[THIS:%.*]] = load %struct.ReadOnlyIntArray*, %struct.ReadOnlyIntArray**
// CHECK:   [[VALUES:%.*]] = getelementptr inbounds %struct.ReadOnlyIntArray, %struct.ReadOnlyIntArray* [[THIS]]
// CHECK:   [[VALUE:%.*]] = getelementptr inbounds [5 x {{i32|i64}}], [5 x {{i32|i64}}]* [[VALUES]]
// CHECK:   ret {{i32|i64}}* [[VALUE]]

public func index(_ arr: inout ReadWriteIntArray, _ arg: Int32, _ val: Int32) { arr[arg] = val }

// CHECK: call [[RES:i32|i64]]* [[NAME:@(_ZN17ReadWriteIntArrayixEi|"\?\?AReadWriteIntArray@@QEAAAEAHH@Z")]](%struct.ReadWriteIntArray* {{%[0-9]+}}, {{i32|i64}} {{%[0-9]+}})
// CHECK: define linkonce_odr nonnull align 4 dereferenceable(4) [[RES]]* [[NAME]](%struct.ReadWriteIntArray* nonnull dereferenceable(20) {{.*}}, {{i32 %x|\[1 x i32\] %x|i64 %x|%struct.ReadWriteIntArray\* byval\(%struct.ReadWriteIntArray\) align 2 %rhs}})
// CHECK:   [[THIS:%.*]] = load %struct.ReadWriteIntArray*, %struct.ReadWriteIntArray**
// CHECK:   [[VALUES:%.*]] = getelementptr inbounds %struct.ReadWriteIntArray, %struct.ReadWriteIntArray* [[THIS]]
// CHECK:   [[VALUE:%.*]] = getelementptr inbounds [5 x {{i32|i64}}], [5 x {{i32|i64}}]* [[VALUES]]
// CHECK:   ret {{i32|i64}}* [[VALUE]]
