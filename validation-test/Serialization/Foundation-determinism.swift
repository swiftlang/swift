// RUN: %target-build-swift -module-name Foundation -parse-as-library %S/../../stdlib/public/SDK/Foundation/*.swift -emit-module-path %t.1.swiftmodule
// RUN: %target-build-swift -module-name Foundation -parse-as-library %S/../../stdlib/public/SDK/Foundation/*.swift -emit-module-path %t.2.swiftmodule
// RUN: diff <(llvm-bcanalyzer -dump %t.1.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g') <(llvm-bcanalyzer -dump %t.2.swiftmodule | sed -e 's/\.[0-9]\.swiftmodule/\.x\.swiftmodule/g')
// XFAIL: *

// REQUIRES: objc_interop

// Compiling the same set of files twice, without modifying them (and without
// generating inlineable SIL) should produce the same swiftmodule. We don't
// promise more than that at this time...
