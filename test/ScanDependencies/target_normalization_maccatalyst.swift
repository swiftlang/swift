// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -parse-stdlib -scan-dependencies %s \
// RUN:   -module-cache-path %t/module-cache \
// RUN:   -I %S/Inputs/target-normalization/iOSSupport \
// RUN:   -I %S/Inputs/target-normalization/macOS \
// RUN:   -target arm64-apple-ios15.0-macabi \
// RUN:   -o %t/deps-arm64-apple-ios-macabi.json

// RUN: %validate-json %t/deps-arm64-apple-ios-macabi.json
// RUN: %FileCheck %s --input-file %t/deps-arm64-apple-ios-macabi.json \
// RUN:   -DORIG_ARCH=arm64 \
// RUN:   -DNORM_ARCH=aarch64

// RUN: %target-swift-frontend -parse-stdlib -scan-dependencies %s \
// RUN:   -module-cache-path %t/module-cache \
// RUN:   -I %S/Inputs/target-normalization/iOSSupport \
// RUN:   -I %S/Inputs/target-normalization/macOS \
// RUN:   -target arm64e-apple-ios15.0-macabi \
// RUN:   -o %t/deps-arm64e-apple-ios-macabi.json

// RUN: %validate-json %t/deps-arm64e-apple-ios-macabi.json
// RUN: %FileCheck %s --input-file %t/deps-arm64e-apple-ios-macabi.json \
// RUN:   -DORIG_ARCH=arm64e \
// RUN:   -DNORM_ARCH=arm64e

import Zippered
import Unzippered

// CHECK:        "modulePath": "{{.*}}Unzippered-[[UNZIPPERED_HASH:[A-Z0-9]+]].swiftmodule",
// CHECK:        "moduleInterfacePath": "{{.*}}/iOSSupport/Unzippered.swiftmodule/arm64e-apple-ios-macabi.swiftinterface",
// CHECK:        "commandLine"
// CHECK:        "-compile-module-from-interface",
// CHECK:        "-target",
// CHECK-NEXT:   "[[ORIG_ARCH]]-apple-ios15.0-macabi"
// CHECK-NOT:    "-target-variant"
// CHECK:        "-target",
// CHECK-NEXT:   "[[NORM_ARCH]]-apple-ios15.0-macabi"
// CHECK-NOT:    "-target-variant"
// CHECK:        "contextHash": "[[UNZIPPERED_HASH]]",

// CHECK:        "modulePath": "{{.*}}Zippered-[[ZIPPERED_HASH:[A-Z0-9]+]].swiftmodule",
// CHECK:        "moduleInterfacePath": "{{.*}}/macOS/Zippered.swiftmodule/arm64e-apple-ios-macabi.swiftinterface",
// CHECK:        "commandLine"
// CHECK:        "-compile-module-from-interface",
// CHECK:        "-target",
// CHECK-NEXT:   "[[ORIG_ARCH]]-apple-ios15.0-macabi"
// CHECK-NOT:    "-target-variant"
// CHECK:        "-target",
// CHECK-NEXT:   "[[NORM_ARCH]]-apple-ios15.0-macabi"
// CHECK-NOT:    "-target-variant"
// CHECK:        "contextHash": "[[ZIPPERED_HASH]]",
