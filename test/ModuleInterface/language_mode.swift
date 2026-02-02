// RUN: %empty-directory(%t)

// RUN: %swift_frontend_plain -target %target-swift-5.1-abi-triple %s \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -emit-module-interface-path %t.swiftinterface \
// RUN:   -emit-module -o /dev/null 2>&1 | %FileCheck %s

// CHECK: <unknown>:0: warning: emitting module interface files requires '-language-mode'

// RUN: %swift_frontend_plain -target %target-swift-5.1-abi-triple %s \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -emit-module-interface-path %t.swiftinterface \
// RUN:   -emit-module -o /dev/null -language-mode 5

// RUN: %FileCheck %s --check-prefix CHECK-SWIFTINTERFACE < %t.swiftinterface

// RUN: %swift_frontend_plain -target %target-swift-5.1-abi-triple %s \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -emit-module-interface-path %t.swiftinterface \
// RUN:   -emit-module -o /dev/null -swift-version 5

// RUN: %FileCheck %s --check-prefix CHECK-SWIFTINTERFACE < %t.swiftinterface

// CHECK-SWIFTINTERFACE: swift-module-flags:
// CHECK-SWIFTINTERFACE-SAME: -enable-library-evolution
// CHECK-SWIFTINTERFACE-SAME: -module-name Test
// CHECK-SWIFTINTERFACE-SAME: {{-swift-version|-language-mode}} 5
