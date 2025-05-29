// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name ImportsSubmodule -I %S/Inputs/submodule-type
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name ImportsSubmodule -I %S/Inputs/submodule-type
// RUN: %FileCheck %s < %t.swiftinterface

import HasSubmodule.Submodule

// CHECK: public func takesHasSubmoduleType(_ x: HasSubmodule.HasSubmoduleType)
public func takesHasSubmoduleType(_ x: HasSubmoduleType) {}

// CHECK: public func takesSubmoduleType(_ x: HasSubmodule.SubmoduleType)
public func takesSubmoduleType(_ x: SubmoduleType) {}
