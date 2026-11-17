// Verifies that a `@cxx @implementation` of a C++ virtual method is emitted
// under its mangled symbol, along with its adjusting thunks and, for a key
// function, the class's vtable, VTT, and RTTI. Swift-side calls dispatch
// statically for a value record and through the importer's thunk for a
// foreign reference type.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -I %S/Inputs \
// RUN:   %s -o %t.ll
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi,CHECK-%target-abi-%target-ptrsize < %t.ll
// RUN: %FileCheck %s --check-prefix=NOVTABLE-%target-abi < %t.ll

// REQUIRES: swift_feature_CxxImplementation

import Virtual


// The classes whose key function is implemented below. The Microsoft ABI has
// no key functions, so Swift emits no vftables.

// CHECK-SYSV: @_ZTV5Shape = {{(dso_local )?}}constant { [5 x ptr] } { [5 x ptr] [ptr null, ptr @_ZTI5Shape, ptr @_ZNK5Shape4areaEv{{(\.ptrauth)?}}, ptr @_ZN5Shape5scaleEi{{(\.ptrauth)?}}, ptr @_ZNK5Shape9perimeterEv{{(\.ptrauth)?}}] }
// CHECK-SYSV: @_ZTI5Shape = {{(dso_local )?}}constant
// CHECK-SYSV: @_ZTS5Shape = {{(dso_local )?}}constant [7 x i8] c"5Shape\00"

// CHECK-SYSV: @_ZTV8Abstract = {{(dso_local )?}}constant { [4 x ptr] } { [4 x ptr] [ptr null, ptr @_ZTI8Abstract, ptr @_ZNK8Abstract6anchorEv{{(\.ptrauth)?}}, ptr @__cxa_pure_virtual{{(\.ptrauth(\.[0-9]+)?)?}}] }

// CHECK-SYSV: @_ZTV9MIDerived = {{(dso_local )?}}constant { [7 x ptr], [3 x ptr] } { [7 x ptr] [ptr null, ptr @_ZTI9MIDerived, ptr @[[MI_COMPLETE_DTOR:_ZN9MIDerivedD[12]Ev]]{{(\.ptrauth)?}}, ptr @_ZN9MIDerivedD0Ev{{(\.ptrauth)?}}, ptr @_ZN9MIDerived6firstAEv{{(\.ptrauth)?}}, ptr @_ZN9MIDerived8miAnchorEv{{(\.ptrauth)?}}, ptr @_ZNK9MIDerived5fromBEv{{(\.ptrauth)?}}],
// CHECK-SYSV-64-SAME: [3 x ptr] [ptr inttoptr (i64 -16 to ptr), ptr @_ZTI9MIDerived, ptr @_ZThn16_NK9MIDerived5fromBEv{{(\.ptrauth)?}}] }
// CHECK-SYSV-32-SAME: [3 x ptr] [ptr inttoptr (i32 -8 to ptr), ptr @_ZTI9MIDerived, ptr @_ZThn8_NK9MIDerived5fromBEv] }
// CHECK-SYSV: @_ZTI9MIDerived = {{(dso_local )?}}constant

// CHECK-SYSV-64: @_ZTV8VDerived = {{(dso_local )?}}constant { [5 x ptr], [4 x ptr] } { [5 x ptr] [ptr inttoptr (i64 16 to ptr), ptr null, ptr @_ZTI8VDerived, ptr @_ZN8VDerived7vAnchorEv{{(\.ptrauth)?}}, ptr @_ZNK8VDerived8vbMethodEv{{(\.ptrauth)?}}], [4 x ptr] [ptr inttoptr (i64 -16 to ptr), ptr inttoptr (i64 -16 to ptr), ptr @_ZTI8VDerived, ptr @_ZTv0_n24_NK8VDerived8vbMethodEv{{(\.ptrauth)?}}] }
// CHECK-SYSV-32: @_ZTV8VDerived = {{(dso_local )?}}constant { [5 x ptr], [4 x ptr] } { [5 x ptr] [ptr inttoptr (i32 8 to ptr), ptr null, ptr @_ZTI8VDerived, ptr @_ZN8VDerived7vAnchorEv, ptr @_ZNK8VDerived8vbMethodEv], [4 x ptr] [ptr inttoptr (i32 -8 to ptr), ptr inttoptr (i32 -8 to ptr), ptr @_ZTI8VDerived, ptr @_ZTv0_n12_NK8VDerived8vbMethodEv] }
// CHECK-SYSV: @_ZTT8VDerived = {{(dso_local )?}}unnamed_addr constant [2 x ptr]

// CHECK-SYSV: @_ZTV6Engine = {{(dso_local )?}}constant { [4 x ptr] } { [4 x ptr] [ptr null, ptr @_ZTI6Engine, ptr @_ZNK6Engine6statusEv{{(\.ptrauth)?}}, ptr @_ZN6Engine5boostEi{{(\.ptrauth)?}}] }

// CHECK-SYSV: @_ZTV14AbstractEngine = {{(dso_local )?}}constant { [4 x ptr] } { [4 x ptr] [ptr null, ptr @_ZTI14AbstractEngine, ptr @_ZNK14AbstractEngine8aeAnchorEv{{(\.ptrauth)?}}, ptr @__cxa_pure_virtual{{(\.ptrauth(\.[0-9]+)?)?}}] }

// The classes whose key function stays in C++.
// NOVTABLE-SYSV-NOT: @_ZTV10SimpleBase
// NOVTABLE-SYSV-NOT: @_ZTV13SimpleDerived
// NOVTABLE-SYSV-NOT: @_ZTV12CloneDerived
// NOVTABLE-SYSV-NOT: @_ZTV5Mixer
// NOVTABLE-SYSV-NOT: @_ZTV5Gauge
// NOVTABLE-WIN-NOT: @"??_7


extension Shape {
  // virtual int Shape::area() const; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK5Shape4areaEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?area@Shape@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func area() -> Int32 { return sides * sides }

  // virtual void Shape::scale(int factor);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN5Shape5scaleEi(ptr {{.*}}%0, i32 {{.*}}%1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?scale@Shape@@UEAAXH@Z"(ptr {{.*}}%0, i32 {{.*}}%1)
  @cxx @implementation
  public mutating func scale(_ factor: Int32) { sides *= factor }

  // The inline Shape::perimeter(), named by the vtable.
  // CHECK-SYSV: define linkonce_odr{{.*}} i32 @_ZNK5Shape9perimeterEv(
}

extension SimpleBase {
  // virtual int SimpleBase::simple() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK10SimpleBase6simpleEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?simple@SimpleBase@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func simple() -> Int32 { return stored }
}

// Single inheritance: no thunk.
extension SimpleDerived {
  // int SimpleDerived::simple() const override;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK13SimpleDerived6simpleEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?simple@SimpleDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func simple() -> Int32 { return stored * 2 }
}

extension Abstract {
  // virtual int Abstract::anchor() const; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK8Abstract6anchorEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?anchor@Abstract@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func anchor() -> Int32 { return 7 }
}

extension CloneDerived {
  // RetC *CloneDerived::clone() override; and its return-adjusting thunk.
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZN12CloneDerived5cloneEv(ptr {{.*}}%0)
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZTch0_h4_N12CloneDerived5cloneEv(ptr {{.*}}%this)
  // CHECK-SYSV:   call {{.*}}ptr @_ZN12CloneDerived5cloneEv(ptr {{.*}}%this1)
  // CHECK-SYSV:   getelementptr inbounds i8, ptr %call, i64 4
  // CHECK-WIN-LABEL: define{{.*}} ptr @"?clone@CloneDerived@@UEAAPEAURetC@@XZ"(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define weak_odr{{.*}} ptr @"?clone@CloneDerived@@QEAAPEAURetB@@XZ"(ptr {{.*}}%this)
  @cxx @implementation
  public mutating func clone() -> UnsafeMutablePointer<RetC> {
    return sharedRetC()
  }
}

extension MIDerived {
  // virtual void MIDerived::miAnchor(); the key function. The vtable names the
  // implicit destructor, so it is emitted too (D1, or D2 with constructor
  // aliases).
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN9MIDerived8miAnchorEv(ptr {{.*}}%0)
  // CHECK-SYSV: define linkonce_odr{{.*}} @[[MI_COMPLETE_DTOR]](
  // CHECK-SYSV: define linkonce_odr{{.*}} void @_ZN9MIDerivedD0Ev(
  // CHECK-WIN-LABEL: define{{.*}} void @"?miAnchor@MIDerived@@UEAAXXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public mutating func miAnchor() {}

  // void MIDerived::firstA() override; primary base: no thunk.
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN9MIDerived6firstAEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} void @"?firstA@MIDerived@@UEAAXXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public mutating func firstA() { a += 100 }

  // int MIDerived::fromB() const override; and its this-adjusting thunk.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK9MIDerived5fromBEv(ptr {{.*}}%0)
  // CHECK-SYSV-64-LABEL: define{{.*}} i32 @_ZThn16_NK9MIDerived5fromBEv(ptr {{.*}}%this)
  // CHECK-SYSV-64:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i64 -16
  // CHECK-SYSV-32-LABEL: define{{.*}} i32 @_ZThn8_NK9MIDerived5fromBEv(ptr {{.*}}%this)
  // CHECK-SYSV-32:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i32 -8
  // CHECK-SYSV:   call {{.*}}i32 @_ZNK9MIDerived5fromBEv(ptr {{.*}}[[ADJUSTED]])
  // FIXME: The Microsoft ABI passes `this` pointing to the MIBaseB subobject,
  // but the definition does not adjust it back to the MIDerived.
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?fromB@MIDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func fromB() -> Int32 { return a + b }
}

extension VDerived {
  // virtual void VDerived::vAnchor(); the key function.
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN8VDerived7vAnchorEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} void @"?vAnchor@VDerived@@UEAAXXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public mutating func vAnchor() {}

  // int VDerived::vbMethod() const override; and its vcall-offset thunk.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK8VDerived8vbMethodEv(ptr {{.*}}%0)
  // CHECK-SYSV-64-LABEL: define{{.*}} i32 @_ZTv0_n24_NK8VDerived8vbMethodEv(ptr {{.*}}%this)
  // CHECK-SYSV-32-LABEL: define{{.*}} i32 @_ZTv0_n12_NK8VDerived8vbMethodEv(ptr {{.*}}%this)
  // CHECK-SYSV:   load ptr, ptr %this1
  // CHECK-SYSV-64:   [[VCALL_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, ptr {{%.*}}, i64 -24
  // CHECK-SYSV-64:   [[VCALL_OFFSET:%.*]] = load i64, ptr [[VCALL_OFFSET_ADDR]]
  // CHECK-SYSV-64:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i64 [[VCALL_OFFSET]]
  // CHECK-SYSV-32:   [[VCALL_OFFSET_ADDR:%.*]] = getelementptr inbounds i8, ptr {{%.*}}, i64 -12
  // CHECK-SYSV-32:   [[VCALL_OFFSET:%.*]] = load i32, ptr [[VCALL_OFFSET_ADDR]]
  // CHECK-SYSV-32:   [[ADJUSTED:%.*]] = getelementptr inbounds i8, ptr %this1, i32 [[VCALL_OFFSET]]
  // CHECK-SYSV:   call {{.*}}i32 @_ZNK8VDerived8vbMethodEv(ptr {{.*}}[[ADJUSTED]])
  // FIXME: The Microsoft ABI passes `this` pointing to the VBase subobject, but
  // the definition does not adjust it back to the VDerived.
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?vbMethod@VDerived@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func vbMethod() -> Int32 { return vd }
}

extension Engine {
  // virtual int Engine::status() const; the key function. `self` is `this`.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6Engine6statusEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?status@Engine@@UEBAHXZ"(ptr {{.*}}%0)
  // CHECK: getelementptr inbounds{{.*}} %TSo6EngineV, ptr %0
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  // virtual void Engine::boost(int amount);
  // CHECK-SYSV-LABEL: define{{.*}} void @_ZN6Engine5boostEi(ptr {{.*}}%0, i32 {{.*}}%1)
  // CHECK-WIN-LABEL: define{{.*}} void @"?boost@Engine@@UEAAXH@Z"(ptr {{.*}}%0, i32 {{.*}}%1)
  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }
}

extension AbstractEngine {
  // virtual int AbstractEngine::aeAnchor() const; the key function.
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK14AbstractEngine8aeAnchorEv(ptr {{.*}}%0)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?aeAnchor@AbstractEngine@@UEBAHXZ"(ptr {{.*}}%0)
  @cxx @implementation
  public func aeAnchor() -> Int32 { return 11 }
}

// Overloaded virtual methods: Swift defines the `int` overloads only; the
// `double` ones stay in C++.
extension Mixer {
  // virtual int Mixer::mix(int amount) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK5Mixer3mixEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?mix@Mixer@@UEBAHH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func mix(_ amount: Int32) -> Int32 { return level + amount }
}

extension Gauge {
  // virtual int Gauge::read(int scale) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK5Gauge4readEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"?read@Gauge@@UEBAHH@Z"(ptr %0, i32 %1)
  @cxx @implementation
  public func read(_ scale: Int32) -> Int32 { return level * scale }
}


// Swift-side calls: a value record's virtual method dispatches statically to
// the method symbol itself; a foreign reference type's dispatches dynamically
// through the importer's synthesized thunk.

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}16callVirtualFuncsys5Int32VSo5ShapeVz_So6EngineVtF"
// CHECK-SYSV:   invoke void @_ZN5Shape5scaleEi(ptr {{.*}}%0, i32 {{.*}}2)
// CHECK-SYSV:   invoke void @_ZN6Engine30__synthesizedVirtualCall_boostEi(ptr %1, i32 3)
// CHECK-SYSV:   invoke i32 @_ZNK5Shape4areaEv(ptr %0)
// CHECK-SYSV:   invoke i32 @_ZNK6Engine31__synthesizedVirtualCall_statusEv(ptr %1)
public func callVirtualFuncs(_ s: inout Shape, _ e: Engine) -> Int32 {
  s.scale(2)
  e.boost(3)
  return s.area() + e.status()
}

// CHECK-SYSV: define linkonce_odr{{.*}} void @_ZN6Engine30__synthesizedVirtualCall_boostEi
// CHECK-SYSV: define linkonce_odr{{.*}} i32 @_ZNK6Engine31__synthesizedVirtualCall_statusEv

// Both overloads stay callable from Swift: the Swift-implemented one and the
// one left to C++, which is only declared here.

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}26callOverloadedVirtualFuncsys5Int32VSo5MixerV_So5GaugeVtF"
// CHECK-SYSV:   invoke i32 @_ZNK5Mixer3mixEi(ptr %0, i32 1)
// CHECK-SYSV:   invoke i32 @_ZNK5Mixer3mixEd(ptr %0, double 2.000000e+00)
// CHECK-SYSV:   invoke i32 @_ZNK5Gauge29__synthesizedVirtualCall_readEi(ptr %1, i32 3)
// CHECK-SYSV:   invoke i32 @_ZNK5Gauge29__synthesizedVirtualCall_readEd(ptr %1, double 4.000000e+00)
public func callOverloadedVirtualFuncs(_ m: Mixer, _ g: Gauge) -> Int32 {
  return m.mix(Int32(1)) + m.mix(2.0) + g.read(Int32(3)) + g.read(4.0)
}

// CHECK-SYSV: declare{{.*}} i32 @_ZNK5Mixer3mixEd(
// CHECK-SYSV: define linkonce_odr{{.*}} i32 @_ZNK5Gauge29__synthesizedVirtualCall_readEi
// CHECK-SYSV: define linkonce_odr{{.*}} i32 @_ZNK5Gauge29__synthesizedVirtualCall_readEd
