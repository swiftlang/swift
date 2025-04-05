// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -F %S/Inputs/frameworks -module-cache-path %t/mcp1  -target %target-stable-abi-triple

// REQUIRES: objc_interop

import Module
import Module_Private.Sub4

@_objcImplementation extension Module {
  // expected-warning@-1 {{extension for main class interface does not provide all required implementations}}
  // expected-note@-2 {{missing class method 'version()'}}
  // expected-note@-3 {{missing class method 'alloc()'}}
  // expected-note@-4 {{add stubs for missing '@implementation' requirements}} {{40-40=\n    @objc(version)\n    open class func version() -> UnsafePointer<CChar>! {\n        <#code#>\n    \}\n\n    @objc(alloc)\n    open class func alloc() -> Self! {\n        <#code#>\n    \}\n}}
}

extension Module: @retroactive ModuleProto {} // no-error
