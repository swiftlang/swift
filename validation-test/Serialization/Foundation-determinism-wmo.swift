// RUN: %target-build-swift -module-name Foundation -parse-as-library %S/../../stdlib/public/SDK/Foundation/*.swift %T/../../../stdlib/public/SDK/Foundation/8/*.swift -emit-module-path %t.1.swiftmodule -force-single-frontend-invocation
// RUN: %target-build-swift -module-name Foundation -parse-as-library %S/../../stdlib/public/SDK/Foundation/*.swift %T/../../../stdlib/public/SDK/Foundation/8/*.swift -emit-module-path %t.2.swiftmodule -force-single-frontend-invocation
// RUN: diff <(llvm-bcanalyzer -dump %t.1.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g') <(llvm-bcanalyzer -dump %t.2.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g')

// REQUIRES: sr4342
// REQUIRES: objc_interop
// REQUIRES: PTRSIZE=64

// Compiling the same set of files twice, without modifying them (and without
// generating inlineable SIL) should produce the same swiftmodule. We don't
// promise more than that at this time...

// This test (and Foundation-determinism.swift) are known to be rather
// brittle, since they refer directly to sources in stdlib/public/ as well as
// gyb outputs in the build folder, and assume the Apple Foundation overlay can
// be built with a relatively simple command line. If it ever breaks, feel free
// to just disable it and file an SR.
