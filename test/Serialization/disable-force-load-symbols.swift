// Test that -disable-force-load-symbols propagates through serialized
// LINK_LIBRARY records (and through swift-module-flags in .swiftinterface),
// so consumers automatically skip the
// _swift_FORCE_LOAD_$_<module>_$_<consumer> reference at IRGen.
//
// See also: test/ModuleInterface/force-load-autolink.swift (positive case).

// RUN: %empty-directory(%t)

// === 1. Producer
// RUN: %target-swift-frontend -emit-module -parse-stdlib                       \
// RUN:   -module-name TestModule -module-link-name TestModule                  \
// RUN:   -autolink-force-load -disable-force-load-symbols                      \
// RUN:   -o %t/TestModule.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-ide-test -print-module-metadata                           \
// RUN:   -module-to-print TestModule -I %t -source-filename %s                 \
// RUN:   | %FileCheck --check-prefix=DISABLED %s
//
// DISABLED: link library: TestModule, force load: false

// === 2. Consumer
// RUN: %target-swift-frontend -emit-ir -parse-stdlib                           \
// RUN:   -runtime-compatibility-version none                                   \
// RUN:   -disable-autolinking-runtime-compatibility-dynamic-replacements       \
// RUN:   -disable-autolinking-runtime-compatibility-concurrency                \
// RUN:   -I %t %s | %FileCheck --check-prefix=CONSUMER %s
//
// CONSUMER-NOT: _swift_FORCE_LOAD_$_TestModule

// === 3. Producer
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -parse-stdlib                         \
// RUN:   -module-name TestModule -module-link-name TestModule                  \
// RUN:   -enable-library-evolution                                             \
// RUN:   -autolink-force-load -disable-force-load-symbols                      \
// RUN:   -emit-module-interface-path %t/TestModule.swiftinterface              \
// RUN:   %S/../Inputs/empty.swift
// RUN: %FileCheck --check-prefix=INTERFACE-FLAG %s < %t/TestModule.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface                   \
// RUN:   -o %t/TestModule.swiftmodule %t/TestModule.swiftinterface
// RUN: %target-swift-ide-test -print-module-metadata                           \
// RUN:   -module-to-print TestModule -I %t -source-filename %s                 \
// RUN:   | %FileCheck --check-prefix=INTERFACE-DISABLED %s
//
// INTERFACE-FLAG: swift-module-flags:{{.*}}-disable-force-load-symbols
// INTERFACE-DISABLED: link library: TestModule, force load: false

// === 4. Negative control
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-stdlib                       \
// RUN:   -module-name TestModule -module-link-name TestModule                  \
// RUN:   -autolink-force-load                                                  \
// RUN:   -o %t/TestModule.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-ide-test -print-module-metadata                           \
// RUN:   -module-to-print TestModule -I %t -source-filename %s                 \
// RUN:   | %FileCheck --check-prefix=BASELINE %s
// RUN: %target-swift-frontend -emit-ir -parse-stdlib                           \
// RUN:   -runtime-compatibility-version none                                   \
// RUN:   -disable-autolinking-runtime-compatibility-dynamic-replacements       \
// RUN:   -disable-autolinking-runtime-compatibility-concurrency                \
// RUN:   -I %t %s | %FileCheck --check-prefix=BASELINE-CONSUMER %s
//
// BASELINE: link library: TestModule, force load: true
// BASELINE-CONSUMER: _swift_FORCE_LOAD_$_TestModule

import TestModule
