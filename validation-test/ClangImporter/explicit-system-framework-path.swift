// RUN: %target-swift-frontend -F %S/Inputs/explicit-system-framework-path/sdkroot/Library/Frameworks -F %S/Inputs/explicit-system-framework-path/override -typecheck -sdk %S/Inputs/explicit-system-framework-path/sdkroot %s 2>&1 | %FileCheck %s

// Make sure we're matching Clang's behavior on this.
// RUN: echo '@import TestUser;' | %clang -F %S/Inputs/explicit-system-framework-path/sdkroot/Library/Frameworks -F %S/Inputs/explicit-system-framework-path/override -isysroot %S/Inputs/explicit-system-framework-path/sdkroot -x objective-c -fmodules -fsyntax-only - 2>&1 | %FileCheck %s

// This only works with framework search paths because normal include paths
// will pop up a "redefinition of module 'X'" error. Only Apple platforms have
// default framework search paths, so just limit this to macOS.
// REQUIRES: OS=macosx

import TestUser

// CHECK: TestUser found in override
// CHECK: Test.h found in override
