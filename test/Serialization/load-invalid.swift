// RUN: rm -rf %t
// RUN: mkdir %t

// RUN: touch %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// RUN: echo -n 'a' > %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// RUN: echo -n 'abcd' > %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

// RUN: echo -n 'abcde' > %t/new_module.swiftmodule
// RUN: %target-swift-frontend %s -parse -I %t -verify -show-diagnostics-after-fatal

import new_module // expected-error{{malformed module file}}

// malformed module files produce an empty module to avoid further errors.
new_module.foo() // expected-error {{module 'new_module' has no member named 'foo'}}
