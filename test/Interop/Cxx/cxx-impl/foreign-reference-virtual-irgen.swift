// `@cxx @implementation` of overrides in foreign reference types: each has its
// own symbol, a key function brings its class's vtable and RTTI, and `super`
// calls the base method directly.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -I %S/Inputs \
// RUN:   %s -o %t.ll
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi,CHECK-%target-abi-%target-ptrsize < %t.ll
// RUN: %FileCheck %s --check-prefix=NOVTABLE-%target-abi < %t.ll

// REQUIRES: swift_feature_CxxImplementation

import ForeignReferenceVirtual


// The Microsoft ABI has no key functions, so it gets no vftables.

// CHECK-SYSV: @_ZTV7Derived = {{(dso_local )?}}constant { [5 x ptr] } { [5 x ptr] [ptr null, ptr @_ZTI7Derived, ptr @_ZNK4Base6anchorEv{{(\.ptrauth(\.[0-9]+)?)?}}, ptr @_ZNK7Derived8describeEv{{(\.ptrauth)?}}, ptr @_ZNK4Base3tagEv{{(\.ptrauth(\.[0-9]+)?)?}}] }
// CHECK-SYSV: @_ZTI7Derived = {{(dso_local )?}}constant { ptr, ptr, ptr } { ptr {{(getelementptr inbounds \(ptr, ptr )?}}@_ZTVN10__cxxabiv120__si_class_type_infoE{{(, i(32|64) 2\)|\.ptrauth)}}, ptr @_ZTS7Derived, ptr @_ZTI4Base }
// CHECK-SYSV: @_ZTS7Derived = {{(dso_local )?}}constant [9 x i8] c"7Derived\00"

// CHECK-SYSV: @_ZTV4Base = {{(dso_local )?}}constant { [5 x ptr] } { [5 x ptr] [ptr null, ptr @_ZTI4Base, ptr @_ZNK4Base6anchorEv{{(\.ptrauth(\.[0-9]+)?)?}}, ptr @_ZNK4Base8describeEv{{(\.ptrauth)?}}, ptr @_ZNK4Base3tagEv{{(\.ptrauth(\.[0-9]+)?)?}}] }
// CHECK-SYSV: @_ZTI4Base = {{(dso_local )?}}constant { ptr, ptr } { ptr {{(getelementptr inbounds \(ptr, ptr )?}}@_ZTVN10__cxxabiv117__class_type_infoE{{(, i(32|64) 2\)|\.ptrauth)}}, ptr @_ZTS4Base }
// CHECK-SYSV: @_ZTS4Base = {{(dso_local )?}}constant [6 x i8] c"4Base\00"

// CHECK-SYSV: @_ZTV4Leaf = {{(dso_local )?}}constant { [5 x ptr] } { [5 x ptr] [ptr null, ptr @_ZTI4Leaf, ptr @_ZNK4Base6anchorEv{{(\.ptrauth(\.[0-9]+)?)?}}, ptr @_ZNK4Leaf8describeEv{{(\.ptrauth)?}}, ptr @_ZNK4Leaf3tagEv{{(\.ptrauth)?}}] }
// CHECK-SYSV: @_ZTI4Leaf = {{(dso_local )?}}constant { ptr, ptr, ptr } { ptr {{(getelementptr inbounds \(ptr, ptr )?}}@_ZTVN10__cxxabiv120__si_class_type_infoE{{(, i(32|64) 2\)|\.ptrauth)}}, ptr @_ZTS4Leaf, ptr @_ZTI7Derived }
// CHECK-SYSV: @_ZTS4Leaf = {{(dso_local )?}}constant [6 x i8] c"4Leaf\00"

// CHECK-SYSV: @_ZTV12MultiDerived = {{(dso_local )?}}constant { [6 x ptr], [3 x ptr] } { [6 x ptr] [ptr null, ptr @_ZTI12MultiDerived, ptr @_ZNK4Base6anchorEv{{(\.ptrauth(\.[0-9]+)?)?}}, ptr @_ZNK12MultiDerived8describeEv{{(\.ptrauth)?}}, ptr @_ZNK4Base3tagEv{{(\.ptrauth(\.[0-9]+)?)?}}, ptr @_ZNK12MultiDerived10fromSecondEv{{(\.ptrauth)?}}],
// CHECK-SYSV-64-SAME: [3 x ptr] [ptr inttoptr (i64 -16 to ptr), ptr @_ZTI12MultiDerived, ptr @_ZThn16_NK12MultiDerived10fromSecondEv{{(\.ptrauth)?}}] }
// CHECK-SYSV-32-SAME: [3 x ptr] [ptr inttoptr (i32 -8 to ptr), ptr @_ZTI12MultiDerived, ptr @_ZThn8_NK12MultiDerived10fromSecondEv] }
// CHECK-SYSV: @_ZTI12MultiDerived = {{(dso_local )?}}constant { {{.*}} } { ptr {{(getelementptr inbounds \(ptr, ptr )?}}@_ZTVN10__cxxabiv121__vmi_class_type_infoE{{(, i(32|64) 2\)|\.ptrauth)}}, ptr @_ZTS12MultiDerived, i32 0, i32 2, ptr @_ZTI4Base, {{i32|i64}} 2, ptr @_ZTI10SecondBase, {{i32|i64}} {{[0-9]+}} }
// CHECK-SYSV: @_ZTS12MultiDerived = {{(dso_local )?}}constant [15 x i8] c"12MultiDerived\00"

// CHECK-SYSV: @_ZTV15ConcreteDerived = {{(dso_local )?}}constant { [4 x ptr] } { [4 x ptr] [ptr null, ptr @_ZTI15ConcreteDerived, ptr @_ZNK12AbstractBase14abstractAnchorEv{{(\.ptrauth)?}}, ptr @_ZNK15ConcreteDerived4pureEv{{(\.ptrauth)?}}] }
// CHECK-SYSV: @_ZTI15ConcreteDerived = {{(dso_local )?}}constant { ptr, ptr, ptr } { ptr {{(getelementptr inbounds \(ptr, ptr )?}}@_ZTVN10__cxxabiv120__si_class_type_infoE{{(, i(32|64) 2\)|\.ptrauth)}}, ptr @_ZTS15ConcreteDerived, ptr @_ZTI12AbstractBase }
// CHECK-SYSV: @_ZTS15ConcreteDerived = {{(dso_local )?}}constant [18 x i8] c"15ConcreteDerived\00"
// CHECK-SYSV: @_ZTI12AbstractBase = external {{(dso_local )?}}constant ptr

// SecondBase's and AbstractBase's key functions stay in C++.
// NOVTABLE-SYSV-NOT: @_ZTV10SecondBase =
// NOVTABLE-SYSV-NOT: @_ZTV12AbstractBase =
// NOVTABLE-WIN-NOT: @"??_7


// `super` calls Base::describe() before it is defined.
extension Derived {
  // int Derived::describe() const override; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Derived8describeEv(ptr {{.*}}%0)
  // CHECK-SYSV-NOT:   __synthesizedVirtualCall
  // CHECK-SYSV:       invoke i32 @_ZNK4Base8describeEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?describe@Derived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() * 2 }
}

extension Base {
  // virtual int Base::describe() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Base8describeEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?describe@Base@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func describe() -> Int32 { return value }

  // virtual int Base::anchor() const; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Base6anchorEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?anchor@Base@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func anchor() -> Int32 { return 0 }
}

extension Derived {
  // int Derived::hide() const; hides Base::hide().
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK7Derived4hideEv(ptr {{.*}}%0)
  // CHECK-SYSV:       invoke i32 @_ZNK4Base4hideEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?hide@Derived@@QEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func hide() -> Int32 { return super.hide() + 1 }
}

extension Leaf {
  // int Leaf::describe() const override; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Leaf8describeEv(ptr {{.*}}%0)
  // CHECK-SYSV-NOT:   __synthesizedVirtualCall
  // CHECK-SYSV:       invoke i32 @_ZNK7Derived8describeEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?describe@Leaf@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() + 1 }

  // int Leaf::tag() const override; Derived only inherits Base::tag().
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK4Leaf3tagEv(ptr {{.*}}%0)
  // CHECK-SYSV-NOT:   __synthesizedVirtualCall
  // CHECK-SYSV:       invoke i32 @_ZNK4Base3tagEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?tag@Leaf@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public override func tag() -> Int32 { return super.tag() + 1000 }
}

extension MultiDerived {
  // int MultiDerived::describe() const override; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK12MultiDerived8describeEv(ptr {{.*}}%0)
  // CHECK-SYSV:       invoke i32 @_ZNK4Base8describeEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?describe@MultiDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() * 3 }

  // int MultiDerived::fromSecond() const override; with its this-adjusting
  // thunk.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK12MultiDerived10fromSecondEv(ptr {{.*}}%0)
  // CHECK-SYSV-64-LABEL: define{{.*}} i32 @_ZThn16_NK12MultiDerived10fromSecondEv(ptr {{.*}}%this)
  // CHECK-SYSV-64:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i64 -16
  // CHECK-SYSV-32-LABEL: define{{.*}} i32 @_ZThn8_NK12MultiDerived10fromSecondEv(ptr {{.*}}%this)
  // CHECK-SYSV-32:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i32 -8
  // CHECK-SYSV:   call {{.*}}i32 @_ZNK12MultiDerived10fromSecondEv(ptr {{.*}}[[ADJUSTED]])
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?fromSecond@MultiDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func fromSecond() -> Int32 { return value + second }
}

extension ConcreteDerived {
  // int ConcreteDerived::pure() const override; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK15ConcreteDerived4pureEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?pure@ConcreteDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public override func pure() -> Int32 { return 8 }
}
