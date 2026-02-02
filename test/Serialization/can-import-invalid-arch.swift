// RUN: %empty-directory(%t)
// RUN: mkdir %t/Foo.swiftmodule
// RUN: touch %t/Foo.swiftmodule/i387.swiftmodule
// RUN: touch %t/Foo.swiftmodule/ppc65.swiftmodule
// RUN: touch %t/Foo.swiftmodule/i387.swiftdoc
// RUN: touch %t/Foo.swiftmodule/ppc65.swiftdoc
// RUN: %target-swift-frontend %s -typecheck -I %t 2>&1 | %FileCheck %s -DTARGET_ARCHITECTURE=%module-target-triple

// CHECK: {{.*}} warning: could not find module 'Foo' for target '[[TARGET_ARCHITECTURE]]'; found: {{ppc65, i387|i387, ppc65}}, at: {{.*}}Foo.swiftmodule

#if canImport(Foo)
  import Foo
#endif
