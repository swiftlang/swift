// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/TestFmSwift.swift -module-name TestFm -enable-experimental-cxx-interop -F %t -o %t/TestFm.swiftmodule -import-underlying-module

// RUN: %sourcekitd-test -req=interface-gen -module TestFm -- -Xfrontend -disable-implicit-concurrency-module-import -Xfrontend -disable-implicit-string-processing-module-import -I %t -target %target-triple %clang-importer-sdk-nosource | %FileCheck %s

//--- TestFm.framework/Headers/TestFm.h
#pragma once

namespace ns {
    void testFunction();
}

//--- TestFm.framework/Modules/module.modulemap
framework module TestFm {
    umbrella header "TestFm.h"

    export *
    module * { export * }
}

//--- TestFmSwift.swift

public func testSwiftFunc() {
}

// CHECK: public enum ns {
// CHECK:  public static func testFunction()
// CHECK: }

// CHECK: public func testSwiftFunc()
