// 1. Create folders
// RUN: %empty-directory(%t/PrebuiltModule.swiftmodule)
// RUN: %empty-directory(%t/ModuleCache)

// 2. Define some public API
// RUN: echo 'public struct InPrebuiltModule {}' > %t/PrebuiltModule.swift

// 3. Compile textual interface only into a directory
// RUN: %target-swift-frontend -emit-module %t/PrebuiltModule.swift -module-name PrebuiltModule -emit-module-interface-path %t/PrebuiltModule.swiftmodule/%target-swiftinterface-name

// 4. Import this module with -disable-building-interface should fail
// RUN: not %target-swift-frontend -typecheck -I %t %s -module-cache-path %t/ModuleCache -sdk %t -disable-building-interface

// 5. Import this module without -disable-building-interface should succeed
// RUN: %target-swift-frontend -typecheck -I %t %s -module-cache-path %t/ModuleCache -sdk %t

import PrebuiltModule

func x<T>(_ x: T) {}

x(InPrebuiltModule.self)
