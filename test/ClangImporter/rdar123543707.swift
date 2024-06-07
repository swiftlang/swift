// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -F %S/Inputs/frameworks -module-cache-path %t/mcp1  -target %target-stable-abi-triple

// REQUIRES: objc_interop

import Module
import Module_Private.Sub4

@_objcImplementation extension Module {
  // expected-warning@-1 {{extension for main class interface should provide implementation for class method 'version()'}}
  // expected-warning@-2 {{extension for main class interface should provide implementation for class method 'alloc()'}}
}

extension Module: @retroactive ModuleProto {} // no-error
