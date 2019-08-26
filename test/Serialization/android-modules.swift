// RUN: %empty-directory(%t)
// RUN: %swift_driver_plain --driver-mode=swiftc -target aarch64-unknown-linux-android -c %s -parse-stdlib -module-name Swift -emit-module -emit-module-path %t/Swift.swiftmodule
// RUN: %swift_driver_plain --driver-mode=swiftc -O -target aarch64-unknown-linux-android21 -c %s -I %t

