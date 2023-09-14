// RUN: rm -f %t.*

// Generate a bridging PCH, use it in a swift file, and check that the swift file's .swiftdeps
// mention the .h the PCH was generated from, and any .h files included in it.

// RUN: %target-swift-frontend -emit-pch -o %t.pch %/S/Inputs/chained-unit-test-bridging-header-to-pch.h
// RUN: %target-swift-frontend -module-name test -c -emit-dependencies-path %t.d -emit-reference-dependencies-path %t.swiftdeps -primary-file %s -import-objc-header %t.pch
// RUN: %FileCheck --check-prefix CHECK-DEPS %s < %t.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t.swiftdeps > %t-processed.swiftdeps
// RUN: %FileCheck --check-prefix CHECK-SWIFTDEPS --enable-yaml-compatibility %s < %t-processed.swiftdeps
// RUN: %FileCheck --check-prefix CHECK-SWIFTDEPS2 --enable-yaml-compatibility %s < %t-processed.swiftdeps

// RUN: %target-swift-frontend -module-name test -c -emit-dependencies-path %t.persistent.d -emit-reference-dependencies-path %t.persistent.swiftdeps -primary-file %s -import-objc-header %/S/Inputs/chained-unit-test-bridging-header-to-pch.h -pch-output-dir %t/pch
// RUN: %FileCheck --check-prefix CHECK-DEPS %s < %t.persistent.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t.persistent.swiftdeps > %t-processed.persistent.swiftdeps
// RUN: %FileCheck --check-prefix CHECK-SWIFTDEPS  --enable-yaml-compatibility %s < %t-processed.persistent.swiftdeps
// RUN: %FileCheck --check-prefix CHECK-SWIFTDEPS2 --enable-yaml-compatibility %s < %t-processed.persistent.swiftdeps

print(app_function(1))

// CHECK-DEPS: pch-bridging-header-deps-fine.o : {{.*}}{{/|\\}}test{{/|\\}}ClangImporter{{/|\\}}Inputs{{/|\\}}app-bridging-header-to-pch.h {{.*}}{{/|\\}}test{{/|\\}}ClangImporter{{/|\\}}Inputs{{/|\\}}chained-unit-test-bridging-header-to-pch.h

// CHECK-SWIFTDEPS: externalDepend {{.*}} '{{.*}}{{/|\\}}test{{/|\\}}ClangImporter{{/|\\}}Inputs{{/|\\}}app-bridging-header-to-pch.h'
// CHECK-SWIFTDEPS: externalDepend {{.*}} '{{.*}}{{/|\\}}test{{/|\\}}ClangImporter{{/|\\}}Inputs{{/|\\}}chained-unit-test-bridging-header-to-pch.h'

// CHECK-SWIFTDEPS2-NOT: {{.*}}.pch
