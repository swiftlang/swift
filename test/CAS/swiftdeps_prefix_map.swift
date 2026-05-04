// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name B -o %t/B.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/B.swift

// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache %t/foo.swift %t/bar.swift \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -scanner-prefix-map-paths %t /^tmp -I %t \
// RUN:   -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas -swift-version 5

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %target-swift-frontend-plain \
// RUN:   -c -cache-compile-job -cas-path %t/cas -module-name Test \
// RUN:   -disable-implicit-swift-modules -o %t/foo.o \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-reference-dependencies-path %t/foo.swiftdeps -emit-dependencies \
// RUN:   -primary-file /^tmp/foo.swift /^tmp/bar.swift @%t/Test.cmd

// RUN: %target-swift-frontend-plain \
// RUN:   -c -cache-compile-job -cas-path %t/cas -module-name Test \
// RUN:   -disable-implicit-swift-modules -o %t/bar.o \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -emit-reference-dependencies-path %t/bar.swiftdeps -emit-dependencies \
// RUN:   /^tmp/foo.swift -primary-file /^tmp/bar.swift @%t/Test.cmd

// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/foo.swiftdeps | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-FOO
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps_with_fingerprints.py %swift-dependency-tool %t/bar.swiftdeps | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-BAR

// CHECK-DAG: externalDepend interface '' '{{/|\\}}^tmp{{/|\\}}A.h'
// CHECK-DAG: externalDepend interface '' '{{/|\\}}^tmp{{/|\\}}A.swiftinterface'

// CHECK-FOO-DAG: sourceFileProvide implementation '' '/^tmp{{/|\\}}foo.swift'
// CHECK-FOO-DAG: sourceFileProvide interface '' '/^tmp{{/|\\}}foo.swift'

// CHECK-BAR-DAG: sourceFileProvide implementation '' '/^tmp{{/|\\}}bar.swift'
// CHECK-BAR-DAG: sourceFileProvide interface '' '/^tmp{{/|\\}}bar.swift'
// CHECK-BAR-DAG: topLevel interface '' foo

//--- module.modulemap
module A {
  header "A.h"
  export *
}

//--- A.h
void a(void);

//--- A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0

@_exported import A
public struct A {}

//--- B.swift
public func b() {}

//--- foo.swift
import A
func foo() {}

//--- bar.swift
import B

func bar() {
  b()
  foo()
}
