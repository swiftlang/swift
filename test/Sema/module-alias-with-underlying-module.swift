// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -module-name MyModule \
// RUN:     -swift-version 5 \
// RUN:     -o %t/MyModule.swiftmodule \
// RUN:     %t/MyModuleOk.swift -I%t

// RUN: %target-swift-frontend -typecheck -module-name MyModule \
// RUN:     -swift-version 5 \
// RUN:     -o %t/MyModule.swiftmodule \
// RUN:     %t/MyModule.swift -I%t \
// RUN:     -module-alias Mod___MyModule=MyModule

//--- module.modulemap
module MyModule {
    header "MyModule.h"
}

//--- MyModule.h
struct UnderlyingType {};

//--- MyModuleOk.swift

@_exported import MyModule
public func refToUnderlying(_ a: MyModule.UnderlyingType) {}

//--- MyModule.swift

@_exported import Mod___MyModule
public func refToUnderlying(_ a: Mod___MyModule.UnderlyingType) {}
