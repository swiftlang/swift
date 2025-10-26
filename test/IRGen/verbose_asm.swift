// RUN: %target-swift-frontend -emit-assembly %s | %FileCheck %s --check-prefix=VERBOSE
// RUN: %target-swift-frontend -emit-assembly -verbose-asm %s | %FileCheck %s --check-prefix=VERBOSE
// RUN: %target-swift-frontend -emit-assembly -no-verbose-asm %s | %FileCheck %s --check-prefix=NO-VERBOSE

// Test that verbose assembly is enabled by default and can be disabled

func simpleFunction() -> Int {
    return 42
}

// VERBOSE: {{;|#|//|@}} -- {{Begin|End}} function
// NO-VERBOSE-NOT: {{;|#|//|@}} -- {{Begin|End}} function
