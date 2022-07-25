// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

public enum PrimitivePayload {
    case x(Int64)
    case y(Double)
    case z(Bool)
}

public func makePrimitivePayload(_ x: Int) -> PrimitivePayload {
    switch x {
    case 1:
        return .x(9999)
    case 2:
        return .y(3.14)
    default:
        return .z(true)
    }
}

public enum Uvw {
    case one, two ,three
}

public enum Xyz {
    case first(Foo)
    case second(Uvw)
    case third(Bar)
}

public struct Foo {
    public var x: Int64
    public init(x: Int64) {
        self.x = x
    }
}

public struct Bar {
    public var x1, x2, x3, x4, x5, x6: Double
    public init(x1: Double, x2: Double, x3: Double, x4: Double, x5: Double, x6: Double) {
        self.x1 = x1
        self.x2 = x2
        self.x3 = x3
        self.x4 = x4
        self.x5 = x5
        self.x6 = x6
    }
}

public func checkFoo(_ lhs: Foo, _ rhs: Foo) -> Bool {
    return lhs.x == rhs.x
}

public func checkBar(_ lhs: Bar, _ rhs: Bar) -> Bool {
    return lhs.x1 == rhs.x1 && lhs.x2 == rhs.x2 && lhs.x3 == rhs.x3 && lhs.x4 == rhs.x4 && lhs.x5 == rhs.x5 && lhs.x6 == rhs.x6
}

public func checkUvw(_ lhs: Uvw, _ rhs: Uvw) -> Bool {
    return lhs == rhs
}

public func makeXyz(_ x: Int) -> Xyz {
    switch x {
    case 1:
        return .first(Foo(x: 1234))
    case 2:
        return .second(.two)
    default:
        return .third(Bar(x1: 1.1, x2: 2.2, x3: 3.3, x4: 4.4, x5: 5.5, x6: 6.6))
    }
}

public func makeUvw(_ x: Int) -> Uvw {
    switch x {
    case 1:
        return .one
    case 2:
        return .two
    default:
        return .three
    }
}

// CHECK: class _impl_Bar {
// CHECK:      static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums3BarVMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }

// CHECK: class _impl_Foo {
// CHECK:      static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums3FooVMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }

// CHECK: class PrimitivePayload final {
// CHECK:      inline int64_t getX() const {
// CHECK-NEXT:   if (!isX()) abort();
// CHECK-NEXT:   alignas(PrimitivePayload) unsigned char buffer[sizeof(PrimitivePayload)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) PrimitivePayload(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   int64_t result;
// CHECK-NEXT:   memcpy(&result, payloadFromDestruction, sizeof(result));
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
// CHECK:      inline double getY() const {
// CHECK-NEXT:   if (!isY()) abort();
// CHECK-NEXT:   alignas(PrimitivePayload) unsigned char buffer[sizeof(PrimitivePayload)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) PrimitivePayload(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   double result;
// CHECK-NEXT:   memcpy(&result, payloadFromDestruction, sizeof(result));
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
// CHECK:      inline bool getZ() const {
// CHECK-NEXT:   if (!isZ()) abort();
// CHECK-NEXT:   alignas(PrimitivePayload) unsigned char buffer[sizeof(PrimitivePayload)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) PrimitivePayload(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   bool result;
// CHECK-NEXT:   memcpy(&result, payloadFromDestruction, sizeof(result));
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
// CHECK: private:
// CHECK:      inline char * _Nonnull _destructiveProjectEnumData() {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums16PrimitivePayloadOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   enumVWTable->destructiveProjectEnumData(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:   return _getOpaquePointer();
// CHECK-NEXT: }
// CHECK: class _impl_PrimitivePayload {
// CHECK:      static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums16PrimitivePayloadOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }

// CHECK: class _impl_Uvw {
// CHECK:      static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums3UvwOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }

// CHECK: class Xyz final {
// CHECK:      inline Foo getFirst() const {
// CHECK-NEXT:   if (!isFirst()) abort();
// CHECK-NEXT:   alignas(Xyz) unsigned char buffer[sizeof(Xyz)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) Xyz(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   return _impl::_impl_Foo::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::_impl_Foo::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:   });
// CHECK-NEXT: }
// CHECK:      inline Bar getThird() const {
// CHECK-NEXT:   if (!isThird()) abort();
// CHECK-NEXT:   alignas(Xyz) unsigned char buffer[sizeof(Xyz)];
// CHECK-NEXT:   auto *thisCopy = new(buffer) Xyz(*this);
// CHECK-NEXT:   char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:   return _impl::_impl_Bar::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::_impl_Bar::initializeWithTake(result, payloadFromDestruction);
// CHECK-NEXT:   });
// CHECK-NEXT: }
// CHECK: private:
// CHECK:      inline char * _Nonnull _destructiveProjectEnumData() {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums3XyzOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   const auto *enumVWTable = reinterpret_cast<swift::_impl::EnumValueWitnessTable *>(vwTable);
// CHECK-NEXT:   enumVWTable->destructiveProjectEnumData(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:   return _getOpaquePointer();
// CHECK-NEXT: }
// CHECK: class _impl_Xyz {
// CHECK:      static inline void initializeWithTake(char * _Nonnull destStorage, char * _Nonnull srcStorage) {
// CHECK-NEXT:   auto metadata = _impl::$s5Enums3XyzOMa(0);
// CHECK-NEXT:   auto *vwTableAddr = reinterpret_cast<swift::_impl::ValueWitnessTable **>(metadata._0) - 1;
// CHECK-NEXT: #ifdef __arm64e__
// CHECK-NEXT:   auto *vwTable = reinterpret_cast<swift::_impl::ValueWitnessTable *>(ptrauth_auth_data(reinterpret_cast<void *>(*vwTableAddr), ptrauth_key_process_independent_data, ptrauth_blend_discriminator(vwTableAddr, 11839)));
// CHECK-NEXT: #else
// CHECK-NEXT:   auto *vwTable = *vwTableAddr;
// CHECK-NEXT: #endif
// CHECK-NEXT:   vwTable->initializeWithTake(destStorage, srcStorage, metadata._0);
// CHECK-NEXT: }
