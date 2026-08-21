// RUN: %target-typecheck-verify-swift -I %S/Inputs -strict-memory-safety -cxx-interoperability-mode=default -diagnostic-style llvm 2>&1

import VirtualMethodAttrs

@available(SwiftStdlib 5.8, *)
func useVirtualMethods(_ x: MyUnsafeReferenceType) {
    x.virtualSafeMethod()

    x.nonvirtualSafeMethod()

    x.virtualUnsafeMethod() // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}}
    // expected-note@-1 {{reference to parameter 'x' involves unsafe type 'MyUnsafeReferenceType'}}
    // expected-note@-2 {{argument 'self' in call to instance method 'virtualUnsafeMethod' has unsafe type 'MyUnsafeReferenceType'}}
}
