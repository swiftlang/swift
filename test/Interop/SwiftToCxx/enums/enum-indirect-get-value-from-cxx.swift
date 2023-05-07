// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function)

@_expose(Cxx)
public class C {
    public var x: UInt64
    public init(x: UInt64) { self.x = x }
}

@_expose(Cxx)
public struct S {
    public var x: UInt64
    public init(x: UInt64) { self.x = x}
}

@_expose(Cxx)
public enum E {
    case empty
    case ui64(UInt64)
    case c(C)
    indirect case e(E)
}

// CHECK:       SWIFT_INLINE_THUNK E E::getE() const {
// CHECK-NEXT:    if (!isE()) abort();
// CHECK-NEXT:    alignas(E) unsigned char buffer[sizeof(E)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) E(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    auto toReturn = swift::_impl::implClassFor<E>::type::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:      swift::_impl::implClassFor<E>::type::initializeWithCopy(result, static_cast<char * _Nonnull>(actualPayload));
// CHECK-NEXT:    });
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return toReturn;
// CHECK-NEXT:  }

@_expose(Cxx)
public indirect enum IndirectEnum {
    case empty
    case ui64(UInt64)
    case d(Double)
    case s(S)
    case c(C)
    case ie(IndirectEnum)
}

// CHECK:       SWIFT_INLINE_THUNK uint64_t IndirectEnum::getUi64() const {
// CHECK-NEXT:    if (!isUi64()) abort();
// CHECK-NEXT:    alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) IndirectEnum(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    uint64_t result;
// CHECK-NEXT:    memcpy(&result, actualPayload, sizeof(result));
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return result;
// CHECK-NEXT:  }

// CHECK:       SWIFT_INLINE_THUNK double IndirectEnum::getD() const {
// CHECK-NEXT:    if (!isD()) abort();
// CHECK-NEXT:    alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) IndirectEnum(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    double result;
// CHECK-NEXT:    memcpy(&result, actualPayload, sizeof(result));
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return result;
// CHECK-NEXT:  }

// CHECK:       SWIFT_INLINE_THUNK S IndirectEnum::getS() const {
// CHECK-NEXT:    if (!isS()) abort();
// CHECK-NEXT:    alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) IndirectEnum(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    auto toReturn = swift::_impl::implClassFor<S>::type::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:      swift::_impl::implClassFor<S>::type::initializeWithCopy(result, static_cast<char * _Nonnull>(actualPayload));
// CHECK-NEXT:    });
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return toReturn;
// CHECK-NEXT:  }

// CHECK:       SWIFT_INLINE_THUNK C IndirectEnum::getC() const {
// CHECK-NEXT:    if (!isC()) abort();
// CHECK-NEXT:    alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) IndirectEnum(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    void * _Nonnull actualObject = *reinterpret_cast<void ** _Nonnull>(actualPayload);
// CHECK-NEXT:    ::swift::_impl::swift_retain(actualObject);
// CHECK-NEXT:    auto toReturn = swift::_impl::implClassFor<C>::type::makeRetained(actualObject);
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return toReturn;
// CHECK-NEXT:  }

// CHECK:       SWIFT_INLINE_THUNK IndirectEnum IndirectEnum::getIe() const {
// CHECK-NEXT:    if (!isIe()) abort();
// CHECK-NEXT:    alignas(IndirectEnum) unsigned char buffer[sizeof(IndirectEnum)];
// CHECK-NEXT:    auto *thisCopy = new(buffer) IndirectEnum(*this);
// CHECK-NEXT:    char * _Nonnull payloadFromDestruction = thisCopy->_destructiveProjectEnumData();
// CHECK-NEXT:    void ** _Nonnull refCountedBox = reinterpret_cast<void ** _Nonnull>(payloadFromDestruction);
// CHECK-NEXT:    void * _Nonnull actualPayload = ::swift::_impl::swift_projectBox(*refCountedBox);
// CHECK-NEXT:    auto toReturn = swift::_impl::implClassFor<IndirectEnum>::type::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:      swift::_impl::implClassFor<IndirectEnum>::type::initializeWithCopy(result, static_cast<char * _Nonnull>(actualPayload));
// CHECK-NEXT:    });
// CHECK-NEXT:    ::swift::_impl::swift_release(*refCountedBox);
// CHECK-NEXT:    return toReturn;
// CHECK-NEXT:  }

@_expose(Cxx)
public func getIndirectEnum(_ x: Int) -> IndirectEnum {
    switch x {
    case 1:
        return .ui64(1234)
    case 2:
        return .d(3.14159)
    case 3:
        return .s(S(x: 5678))
    default:
        return .empty
    }
}

@_expose(Cxx)
public func getIndirectEnumFromClass(_ c: C) -> IndirectEnum {
    return .c(c)
}

@_expose(Cxx)
public func getIndirectEnumFromIndirectEnum(_ ie: IndirectEnum) -> IndirectEnum {
    return .ie(ie)
}
