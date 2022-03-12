// RUN: %empty-directory(%t)
// RUN:     %target-swift-frontend %s -D MOD1 -experimental-hermetic-seal-at-link -emit-module -o %t/Mod1.swiftmodule
// RUN: not %target-swift-frontend %s -D MOD2                                     -emit-module -o %t/Mod2.swiftmodule -I %t 2>&1 | %FileCheck %s
// RUN:     %target-swift-frontend %s -D MOD2 -experimental-hermetic-seal-at-link -emit-module -o %t/Mod2.swiftmodule -I %t

// CHECK: {{.*}} error: module 'Mod1' was built with -experimental-hermetic-seal-at-link, but current compilation does not have -experimental-hermetic-seal-at-link

#if MOD1
    public func foo() {}
#endif

#if MOD2
    import Mod1
    public func bar() {}
#endif
