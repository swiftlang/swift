// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %S/Inputs/use-module-a-impl-only.swift -I %S/Inputs/ -module-name UseModuleAImplOnly -emit-module -emit-module-path %t/UseModuleAImplOnly.swiftmodule -cxx-interoperability-mode=default -enable-library-evolution

// RUN: %target-swift-frontend %s -typecheck -module-name TestMod -I %t -I %S/Inputs

import UseModuleAImplOnly

public func testCallsAPI() {
    testUsesA()
}
