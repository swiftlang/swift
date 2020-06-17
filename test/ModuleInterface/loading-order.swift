/// Test the loading order of module interfaces between the SDK and the
/// prebuilt cache. The order should be:
///
/// 1. Local cache (not tested here)
/// 2. Next to the swiftinterface file
/// 3. Prebuilt-module cache

/// Create folders for a) our Swift module, b) the module cache, and c) a
/// fake resource dir with a default prebuilt module cache inside.
// RUN: %empty-directory(%t/MyModule.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/ResourceDir/%target-sdk-name/prebuilt-modules/MyModule.swiftmodule)

/// Define two sets of public API.
// RUN: echo 'public func nextToSwiftinterface() {}' > %t/NextToSwiftinterface.swift
// RUN: echo 'public func prebuiltModule() {}' > %t/PrebuiltModule.swift

/// Compile this into a module in the SDK.
// RUN: %target-swift-frontend -emit-module %t/NextToSwiftinterface.swift -o %t/MyModule.swiftmodule/%target-cpu.swiftmodule -module-name MyModule -parse-stdlib -emit-module-interface-path %t/MyModule.swiftmodule/%target-cpu.swiftinterface

/// Also put a module with a different API into the default prebuilt cache under the same name.
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -o %t/ResourceDir/%target-sdk-name/prebuilt-modules/MyModule.swiftmodule/%target-cpu.swiftmodule -module-name MyModule -parse-stdlib

/// Import this module and expect to use the swiftmodule next to the swiftinterface.
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -D FIRST_NEXT_TO_SWIFTINTERFACE

/// Remove the first swiftmodule and import again to use the prebuilt swiftmodule.
// RUN: rm %t/MyModule.swiftmodule/%target-cpu.swiftmodule
// RUN: %target-swift-frontend -typecheck -resource-dir %t/ResourceDir -I %t %s -parse-stdlib -module-cache-path %t/ModuleCache -sdk %t -D THEN_PREBUILT_MODULE

import MyModule

#if FIRST_NEXT_TO_SWIFTINTERFACE

nextToSwiftinterface()

#elseif THEN_PREBUILT_MODULE

prebuiltModule()

#endif
