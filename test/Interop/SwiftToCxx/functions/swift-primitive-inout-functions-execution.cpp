// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/swift-primitive-inout-functions-cxx-bridging.swift -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h

// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/swift-functions-execution.o
// RUN: %target-interop-build-swift %S/swift-primitive-inout-functions-cxx-bridging.swift -o %t/swift-functions-execution -Xlinker %t/swift-functions-execution.o -module-name Functions -Xfrontend -entry-point-function-name -Xfrontend swiftMain

// RUN: %target-codesign %t/swift-functions-execution
// RUN: %target-run %t/swift-functions-execution

// REQUIRES: executable_test

#include <cassert>
#include "functions.h"

#define VERIFY_INOUT_VALUE(FUNC, TYPENAME, INITIAL_VALUE, EXPECT_VALUE) \
do { \
    TYPENAME variable = INITIAL_VALUE; \
    FUNC(variable); \
    assert(variable == EXPECT_VALUE); \
} while (false);

int main() {
    using namespace Functions;

    // TODO: how to handle platform-dependent types?

    VERIFY_INOUT_VALUE(inOutCBool, bool, true, false);
    VERIFY_INOUT_VALUE(inOutCChar, char, 'a', '\0');
    VERIFY_INOUT_VALUE(inOutCChar16, char16_t, u'a', u'\0');
    VERIFY_INOUT_VALUE(inOutCChar32, char32_t, U'a', U'\0');
    VERIFY_INOUT_VALUE(inOutCDouble, double, 1.0, 0.0);
    VERIFY_INOUT_VALUE(inOutCFloat, float, 1.0f, 0.0f);
    VERIFY_INOUT_VALUE(inOutCInt, int, 1, 0);
    VERIFY_INOUT_VALUE(inOutCLongLong, long long, 1LL, 0LL);
    VERIFY_INOUT_VALUE(inOutCShort, short, 1, 0);
    VERIFY_INOUT_VALUE(inOutCSignedChar, signed char, 1, 0);
    VERIFY_INOUT_VALUE(inOutCUnsignedChar, unsigned char, 1u, 0u);
    VERIFY_INOUT_VALUE(inOutCUnsignedInt, unsigned int, 1U, 0U);
    VERIFY_INOUT_VALUE(inOutCUnsignedLongLong, unsigned long long, 1ULL, 0ULL);
    VERIFY_INOUT_VALUE(inOutCUnsignedShort, unsigned short, 1u, 0u);
    VERIFY_INOUT_VALUE(inOutCWideChar, wchar_t, L'a', L'\0');

    VERIFY_INOUT_VALUE(inOutBool, bool, true, false);

    VERIFY_INOUT_VALUE(inOutDouble, double, 1.0, 0.0);
    VERIFY_INOUT_VALUE(inOutFloat, float, 1.0f, 0.0f);
    VERIFY_INOUT_VALUE(inOutFloat32, float, 1.0f, 0.0f);
    VERIFY_INOUT_VALUE(inOutFloat64, double, 1.0, 0.0);

    VERIFY_INOUT_VALUE(inOutInt, swift::Int, swift::Int{1}, swift::Int{0});
    VERIFY_INOUT_VALUE(inOutInt16, int16_t, 1, 0);
    VERIFY_INOUT_VALUE(inOutInt32, int32_t, 1, 0);
    VERIFY_INOUT_VALUE(inOutInt64, int64_t, 1, 0);
    VERIFY_INOUT_VALUE(inOutInt8, int8_t, 1, 0);

    VERIFY_INOUT_VALUE(inOutUInt, swift::UInt, swift::UInt{1u}, swift::UInt{0u});
    VERIFY_INOUT_VALUE(inOutUInt16, uint16_t, 1u, 0u);
    VERIFY_INOUT_VALUE(inOutUInt32, uint32_t, 1u, 0u);
    VERIFY_INOUT_VALUE(inOutUInt64, uint64_t, 1u, 0u);
    VERIFY_INOUT_VALUE(inOutUInt8, uint8_t, 1u, 0u);

    {
        int x = 1;
        VERIFY_INOUT_VALUE(inOutOpaquePointer, void * _Nonnull, &x, &x);
        VERIFY_INOUT_VALUE(inOutUnsafeMutableRawPointer, void * _Nonnull, &x, &x);
        VERIFY_INOUT_VALUE(inOutUnsafeRawPointer, void const * _Nonnull, &x, &x);
        VERIFY_INOUT_VALUE(roundTwoInOutUnsafeMutableRawPointer, void * _Nullable, &x, nullptr);
    }

    {
        swift::Int x{1}, y{2};
        inOutTwoInt(x, y);
        assert(x == swift::Int{3});
        assert(y == swift::Int{-4});
    }

    {
        bool x = false;
        double y = 6.28;
        inOutTwoParam(x, y);
        assert(x);
        assert(y == 3.14);
    }
}
