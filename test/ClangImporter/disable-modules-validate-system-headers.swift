// RUN: %target-swift-frontend %s -parse -Rclang-importer 2>&1 | %FileCheck %s -check-prefix=DEFAULT
// DEFAULT: -fmodules-validate-system-headers
// DEFAULT-NOT: -fno-modules-validate-system-headers

// RUN: %target-swift-frontend %s -parse -Rclang-importer -disable-modules-validate-system-headers 2>&1 | %FileCheck %s -check-prefix=DISABLE
// DISABLE: -fno-modules-validate-system-headers
// DISABLE-NOT: -fmodules-validate-system-headers
