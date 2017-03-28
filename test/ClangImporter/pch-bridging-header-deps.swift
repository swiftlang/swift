// REQUIRES: objc_interop
// RUN: rm -f %t.*
//
// Generate a bridging PCH, use it in a swift file, and check that the swift file's .swiftdeps
// mention the .h the PCH was generated from, and any .h files included in it.
//
// RUN: %target-swift-frontend -emit-pch -o %t.pch %S/Inputs/chained-unit-test-bridging-header-to-pch.h
// RUN: %target-swift-frontend -module-name test -c -emit-dependencies-path %t.d -emit-reference-dependencies-path %t.swiftdeps -primary-file %s %s -import-objc-header %t.pch
// RUN: %FileCheck --check-prefix CHECK-DEPS %s < %t.d
// RUN: %FileCheck --check-prefix CHECK-SWIFTDEPS %s < %t.swiftdeps

print(app_function(1))

// CHECK-DEPS: pch-bridging-header-deps.o : {{.*}}SOURCE_DIR/test/ClangImporter/Inputs/app-bridging-header-to-pch.h {{.*}}SOURCE_DIR/test/ClangImporter/Inputs/chained-unit-test-bridging-header-to-pch.h

// CHECK-SWIFTDEPS: depends-external:
// CHECK-SWIFTDEPS: - "SOURCE_DIR/test/ClangImporter/Inputs/app-bridging-header-to-pch.h"
// CHECK-SWIFTDEPS: - "SOURCE_DIR/test/ClangImporter/Inputs/chained-unit-test-bridging-header-to-pch.h"

