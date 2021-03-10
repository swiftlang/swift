/// Test the loading order of module interfaces between the SDK and the
/// prebuilt cache. The order should be:
///
/// 1. swiftmodule in the local cache (not tested here)
/// 2. swiftmodule next to the swiftinterface file
/// 3. If it's a private swiftinterface, rebuild the swiftmodule from the private swiftinterface
/// 4. swiftmodule in the prebuilt-module cache
/// 5. Rebuild the swiftmodule from the swiftinterface file and keep in the local cache

/// Create folders for a) our Swift module, b) the module cache, and c) a
/// fake resource dir with a default prebuilt module cache inside.
// RUN: %empty-directory(%t/MyModule.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/MyModule.swiftmodule)

/// Define two sets of public API.
// RUN: echo 'public func nextToSwiftinterface() {}' > %t/NextToSwiftinterface.swift
// RUN: echo 'public func prebuiltModule() {}' > %t/PrebuiltModule.swift

/// Compile this into a module in the SDK.
// RUN: %target-swift-frontend -emit-module %t/NextToSwiftinterface.swift -o %t/MyModule.swiftmodule/%target-swiftmodule-name -module-name MyModule -parse-stdlib -emit-module-interface-path %t/MyModule.swiftmodule/%target-swiftinterface-name -emit-private-module-interface-path %t/MyModule.swiftmodule/%target-private-swiftinterface-name

/// Also put a module with a different API into the default prebuilt cache under the same name to detect when its picked.
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -o %t/ResourceDir/%target-sdk-name/prebuilt-modules/MyModule.swiftmodule/%target-swiftmodule-name -module-name MyModule -parse-stdlib

/// Import this module and expect to use the swiftmodule next to the swiftinterface.
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -D FIRST_NEXT_TO_SWIFTINTERFACE

/// Remove the swiftmodule next to the swiftinterface, the compiler should rebuild from the private swiftinterface.
// RUN: rm %t/MyModule.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -D FIRST_NEXT_TO_SWIFTINTERFACE

/// Remove the private swiftinterface and import again to use the prebuilt swiftmodule.
// RUN: rm %t/MyModule.swiftmodule/%target-private-swiftinterface-name
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -D THEN_PREBUILT_MODULE

import MyModule

#if FIRST_NEXT_TO_SWIFTINTERFACE

nextToSwiftinterface()

#elseif THEN_PREBUILT_MODULE

prebuiltModule()

#endif
