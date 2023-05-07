// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/enum-indirect-creation-from-cxx.swift -typecheck -module-name Enums -clang-header-expose-decls=has-expose-attr -emit-clang-header-path %t/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/enum-indirect-creation-from-cxx.swift -o %t/swift-enums-execution -Xlinker %t/swift-enums-execution.o -module-name Enums -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-enums-execution
// RUN: %target-run %t/swift-enums-execution

// RUN: %empty-directory(%t-evo)

// RUN: %target-swift-frontend %S/enum-indirect-creation-from-cxx.swift -typecheck -module-name Enums -clang-header-expose-decls=has-expose-attr -enable-library-evolution -emit-clang-header-path %t-evo/enums.h

// RUN: %target-interop-build-clangxx -c %s -I %t-evo -o %t-evo/swift-enums-execution.o
// RUN: %target-interop-build-swift %S/enum-indirect-creation-from-cxx.swift -o %t-evo/swift-enums-execution -Xlinker %t-evo/swift-enums-execution.o -module-name Enums -enable-library-evolution -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t-evo/swift-enums-execution
// RUN: %target-run %t-evo/swift-enums-execution

// REQUIRES: executable_test

#include <cassert>
#include "enums.h"

using namespace Enums;

extern "C" size_t swift_retainCount(void * _Nonnull obj) noexcept;

size_t getRetainCount(const C& obj) {
  void *p = swift::_impl::_impl_RefCountedClass::getOpaquePointer(obj);
  return swift_retainCount(p);
}

int main() {
    {
        auto e = IndirectEnum::empty();
        assert(e.isEmpty());
    }
    {
        auto e = IndirectEnum::ui64(1234);
        assert(e.isUi64());
        assert(e.getUi64() == 1234);
    }
    {
        auto e = IndirectEnum::d(3.14159);
        assert(e.isD());
        assert(e.getD() == 3.14159);
    }
    {
        auto e = IndirectEnum::s(S::init(5678));
        assert(e.isS());
        assert(e.getS().getX() == 5678);
    }

    auto c = C::init(9876);
    assert(getRetainCount(c) == 1);
    {
        auto e1 = IndirectEnum::c(c);
        assert(e1.isC());
        assert(e1.getC().getX() == 9876);
        assert(getRetainCount(c) == 2);

        auto e2 = e1;
        assert(e2.isC());
        assert(e2.getC().getX() == 9876);
        assert(getRetainCount(c) == 2);  // refCount of the box got +1, not the actual object

        e2.getC().setX(1111);
        assert(e2.getC().getX() == 1111);
        assert(e1.getC().getX() == 1111);
        assert(c.getX() == 1111);

        auto e3 = IndirectEnum::ie(e2);
        assert(e3.isIe());
        assert(e3.getIe().isC());
        assert(e3.getIe().getC().getX() == 1111);
        assert(getRetainCount(c) == 2);
    }
    assert(getRetainCount(c) == 1);

    return 0;
}
