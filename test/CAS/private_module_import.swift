// Test that importing private Swift and Clang modules from a public module
// produces diagnostics in CAS (explicit module) mode. This validates that
// library level computed by the dependency scanner is propagated through
// the explicit module map and used by the compiler.

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Scan dependencies. The scanner should detect library levels for Swift modules.
// RUN: %target-swift-frontend -scan-dependencies -module-name TestLib \
// RUN:   -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -parse-stdlib \
// RUN:   %t/TestLib.swift -o %t/deps.json -swift-version 5 \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -sdk %t/sdk \
// RUN:   -I %t/sdk/System/Library/Frameworks/swift \
// RUN:   -I %t/sdk/System/Library/PrivateFrameworks/swift \
// RUN:   -F %t/sdk/System/Library/Frameworks \
// RUN:   -F %t/sdk/System/Library/PrivateFrameworks

/// Verify the scanner reports the correct library levels.
// RUN: %FileCheck %s --check-prefix=SCAN --input-file=%t/deps.json
// SCAN: "modulePath": "{{.*}}PrivateClang_Private{{.*}}.pcm"
// SCAN-NEXT:     "libraryLevel": "spi"
// SCAN: "modulePath": "{{.*}}PrivateClang{{.*}}.pcm"
// SCAN-NEXT:     "libraryLevel": "spi"
// SCAN: "modulePath": "{{.*}}PublicClang_Private{{.*}}.pcm"
// SCAN-NEXT:     "libraryLevel": "spi"
// SCAN: "modulePath": "{{.*}}PublicClang{{.*}}.pcm"
// SCAN-NEXT:     "libraryLevel": "api"
// SCAN: "modulePath": "{{.*}}PrivateSwift{{.*}}.swiftmodule"
// SCAN-NEXT:     "libraryLevel": "spi"
// SCAN: "modulePath": "{{.*}}PublicSwift{{.*}}.swiftmodule"
// SCAN-NEXT:     "libraryLevel": "api"

/// Build all dependency modules and generate the response file.
// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas \
// RUN:   %swift_frontend_plain %t/deps.json -o %t/TestLib.cmd

/// Build the main module with -library-level api in CAS mode.
/// This should error on importing private modules publicly.
// RUN: not %target-swift-frontend-plain \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -parse-stdlib \
// RUN:   -module-name TestLib \
// RUN:   -library-level api \
// RUN:   %t/TestLib.swift @%t/TestLib.cmd 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=DIAG

// DIAG-DAG: error: private module 'PrivateSwift' is imported publicly from the public module 'TestLib'
// DIAG-DAG: error: private module 'PrivateClang_Private' is imported publicly from the public module 'TestLib'
// DIAG-DAG: error: private module 'PublicClang_Private' is imported publicly from the public module 'TestLib'
// DIAG-NOT: error: private module 'PublicSwift' is imported publicly
// DIAG-NOT: error: private module 'PublicClang' is imported publicly

//--- sdk/System/Library/Frameworks/swift/PublicSwift.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name PublicSwift -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-library-evolution
public func publicFunc() {}

//--- sdk/System/Library/PrivateFrameworks/swift/PrivateSwift.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name PrivateSwift -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -enable-library-evolution
public func privateFunc() {}

//--- sdk/System/Library/Frameworks/PublicClang.framework/Headers/PublicClang.h
void publicClangFunc(void);

//--- sdk/System/Library/Frameworks/PublicClang.framework/PrivateHeaders/PublicClang_Private.h
void publicClangPrivateFunc(void);

//--- sdk/System/Library/Frameworks/PublicClang.framework/Modules/module.modulemap
framework module PublicClang {
  header "PublicClang.h"
  export *
}

//--- sdk/System/Library/Frameworks/PublicClang.framework/Modules/module.private.modulemap
framework module PublicClang_Private {
  header "PublicClang_Private.h"
  export *
}

//--- sdk/System/Library/PrivateFrameworks/PrivateClang.framework/Headers/PrivateClang.h
void privateClangFunc(void);

//--- sdk/System/Library/PrivateFrameworks/PrivateClang.framework/PrivateHeaders/PrivateClang_Private.h
void privateClangInternalFunc(void);

//--- sdk/System/Library/PrivateFrameworks/PrivateClang.framework/Modules/module.modulemap
framework module PrivateClang {
  header "PrivateClang.h"
  export *
}

//--- sdk/System/Library/PrivateFrameworks/PrivateClang.framework/Modules/module.private.modulemap
framework module PrivateClang_Private {
  header "PrivateClang_Private.h"
  export *
}

//--- TestLib.swift
import PublicSwift
import PrivateSwift
import PublicClang
import PublicClang_Private
import PrivateClang
import PrivateClang_Private
