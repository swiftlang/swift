// 1. Create folders for a) our Swift module, b) the module cache, and c) a
//    fake resource dir with a default prebuilt module cache inside.
// RUN: %empty-directory(%t/PrebuiltModule.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/PrebuiltModule.swiftmodule)

// 2. Define some public API
// RUN: echo 'public struct InPrebuiltModule {}' > %t/PrebuiltModule.swift

// 3. Compile this into a module and put it into the default prebuilt cache
//    location relative to the fake resource dir. Also drop an interface into
//    the build dir.
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -o %t/ResourceDir/%target-sdk-name/prebuilt-modules/PrebuiltModule.swiftmodule/%target-cpu.swiftmodule -module-name PrebuiltModule -parse-stdlib -emit-module-interface-path %t/PrebuiltModule.swiftmodule/%target-cpu.swiftinterface

// 4. Import this prebuilt module, but DON'T pass in -prebuilt-module-cache-path, it should use the implicit one.
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t

// 5. Make sure we installed a forwarding module in the module cache.
// RUN: %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/ModuleCache/PrebuiltModule-*.swiftmodule

import PrebuiltModule

func x<T>(_ x: T) {}

x(InPrebuiltModule.self)
