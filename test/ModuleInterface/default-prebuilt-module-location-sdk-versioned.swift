// 1. Create folders for a) our Swift module, b) the module cache, and c) a
//    fake resource dir with a default prebuilt module cache inside.
// RUN: %empty-directory(%t/PrebuiltModule.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/10.15/PrebuiltModule.swiftmodule)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/PrebuiltModule.swiftmodule)

// 2. Define some public API
// RUN: echo 'public struct InPrebuiltModule {}' > %t/PrebuiltModule.swift

// 3. Compile this into a module and put it into the default SKD-versioned prebuilt cache
//    location relative to the fake resource dir. Also drop an interface into
//    the build dir.
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -o %t/ResourceDir/%target-sdk-name/prebuilt-modules/10.15/PrebuiltModule.swiftmodule/%target-cpu.swiftmodule -module-name PrebuiltModule -parse-stdlib -emit-module-interface-path %t/PrebuiltModule.swiftmodule/%target-cpu.swiftinterface

// 4. Compile this into a module and put it into the default non-versioned prebuilt cache
//    location relative to the fake resource dir. Also drop an interface into
//    the build dir.
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -o %t/ResourceDir/%target-sdk-name/prebuilt-modules/PrebuiltModule.swiftmodule/%target-cpu.swiftmodule -module-name PrebuiltModule -parse-stdlib -emit-module-interface-path %t/PrebuiltModule.swiftmodule/%target-cpu.swiftinterface

// 5. Import this prebuilt module, but DON'T pass in -prebuilt-module-cache-path, it should use the implicit one from the SDK-versioned prebuilt module cache dir.
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -target-sdk-version 10.15

// 6. Make sure we installed a forwarding module in the module cache.
// RUN: %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/ModuleCache/PrebuiltModule-*.swiftmodule

// 7. Remove the prebuilt module from the SDK-versioned prebuilt module cache dir.
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/10.15)

// 8. Import this prebuilt module, it should not find the prebuilt module cache.
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -target-sdk-version 10.15

// 9. Make sure we built a binary module in the module cache.
// RUN: not %{python} %S/ModuleCache/Inputs/check-is-forwarding-module.py %t/ModuleCache/PrebuiltModule-*.swiftmodule

import PrebuiltModule

func x<T>(_ x: T) {}

x(InPrebuiltModule.self)
