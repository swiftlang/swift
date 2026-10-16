// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/TestFmSwift.swift -module-name TestFm -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/TestFm.framework/Headers/TestFm-Swift.h

// RUN: %target-swift-frontend %t/SecondSwift.swift -module-name Second -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/Second.framework/Headers/Second-Swift.h

// RUN: %target-interop-build-clangxx -std=gnu++17 -fmodules -fcxx-modules -c %t/consumer.cpp -F %t -fsyntax-only -Werror=non-modular-include-in-framework-module

// Repeat with both frameworks using the shared standard library header.
// RUN: %empty-directory(%t/include/swift)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -emit-clang-header-path %t/include/swift/Swift.h
// RUN: %target-swift-frontend %t/TestFmSwift.swift -module-name TestFm -cxx-interoperability-mode=default -typecheck -verify -clang-header-expose-module Swift=swift/Swift.h -emit-clang-header-path %t/TestFm.framework/Headers/TestFm-Swift.h
// RUN: %target-swift-frontend %t/SecondSwift.swift -module-name Second -cxx-interoperability-mode=default -typecheck -verify -clang-header-expose-module Swift=swift/Swift.h -emit-clang-header-path %t/Second.framework/Headers/Second-Swift.h
// RUN: %target-interop-build-clangxx -std=c++17 -fmodules -fcxx-modules -fmodules-cache-path=%t/shared-cache -fsyntax-only %t/consumer.cpp -F %t -I %t/include -Werror=non-modular-include-in-framework-module

// Check that a client C++ file can use Swift stdlib APIs from two
// mixed-language C++ and Swift frameworks.

// This will only pass on Darwin with -Werror=non-modular-include-in-framework-module,
// as the SDK is not modularized on other platforms.
// REQUIRES: OS=macosx

//--- TestFm.framework/Headers/TestFm.h
#pragma once

class CxxClass {
public:
    int testMethod() const {
        return 42;
    }
};

//--- TestFm.framework/Modules/module.modulemap
framework module TestFm {
    umbrella header "TestFm.h"

    export *
    module * { export * }
}

module TestFm.Swift {
    header "TestFm-Swift.h"
}

//--- TestFmSwift.swift

public func testSwiftFunc() -> String {
    return ""
}

//--- Second.framework/Headers/Second.h
#pragma once

class CxxSecondClass {
public:
    int testMethodTwo() const {
        return 42;
    }
};

//--- Second.framework/Modules/module.modulemap
framework module Second {
    umbrella header "Second.h"

    export *
    module * { export * }
}

module Second.Swift {
    header "Second-Swift.h"
}

//--- SecondSwift.swift

public func testSwiftFuncTwo() -> String {
    return ""
}

//--- consumer.cpp

#pragma clang module import TestFm
#pragma clang module import Second

void testUseSwiftStdlibAPIsFromTwoFrameworksImportedViaModules() {
    auto swiftString = TestFm::testSwiftFunc();
    auto c1 = swiftString.getCount();
    auto swiftString2 = Second::testSwiftFuncTwo();
    auto c2 = swiftString.getCount();
}
