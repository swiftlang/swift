// RUN: %empty-directory(%t/BuildDir/Lib.framework/Modules/Lib.swiftmodule)
// RUN: %empty-directory(%t/SecondBuildDir/Lib.framework/Modules/Lib.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)

// RUN: echo 'public func showsUpInBothPlaces() {}' > %t/Lib.swift

// 1. Create a .swiftinterface file containing just one API, and put it inside a second build dir (without a .swiftmodule)
// RUN: %target-swift-frontend -typecheck %t/Lib.swift -emit-module-interface-path %t/SecondBuildDir/Lib.framework/Modules/Lib.swiftmodule/%target-cpu.swiftinterface -module-name Lib

// 2. Add a new API to the module, and compile just the serialized version in the build dir.
// RUN: echo 'public func onlyInTheCompiledModule() {}' >> %t/Lib.swift
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/BuildDir/Lib.framework/Modules/Lib.swiftmodule/%target-cpu.swiftmodule -emit-module-interface-path %t/BuildDir/Lib.framework/Modules/Lib.swiftmodule/%target-cpu.swiftinterface -module-name Lib

// 3. Make sure when we compile this test file, we can access both APIs since we'll
//    load the compiled .swiftmodule instead of the .swiftinterface in the SDK.
// RUN: %target-swift-frontend -typecheck %s -F %t/BuildDir -F %t/SecondBuildDir -module-cache-path %t/ModuleCache

// 4. Make sure we didn't compile any .swiftinterfaces into the module cache.
// RUN: ls %t/ModuleCache | not grep 'swiftmodule'

// 5. This should also work if the swiftinterface isn't present in the first build dir.
// RUN: rm %t/BuildDir/Lib.framework/Modules/Lib.swiftmodule/%target-cpu.swiftinterface
// RUN: %target-swift-frontend -typecheck %s -F %t/BuildDir -F %t/SecondBuildDir -module-cache-path %t/ModuleCache

// 6. Make sure we /still/ didn't compile any .swiftinterfaces into the module cache.
// RUN: ls %t/ModuleCache | not grep 'swiftmodule'

import Lib

showsUpInBothPlaces()
onlyInTheCompiledModule()
