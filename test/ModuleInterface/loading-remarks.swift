/// Test the -Rmodule-loading flag.
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/cache)
// RUN: split-file %s %t

/// Create Swift modules to import.
// RUN: %target-swift-emit-module-interface(%t/SwiftDependency.swiftinterface) \
// RUN:   %t/SwiftDependency.swift
// RUN: %target-swift-frontend -emit-module -o %t/SwiftNonResilientDependency.swiftmodule \
// RUN:   -module-name SwiftNonResilientDependency %t/SwiftDependency.swift
// RUN: %target-swift-emit-module-interface(%t/IndirectMixedDependency.swiftinterface) \
// RUN:   %t/IndirectMixedDependency.swift -I %t

/// Use -Rmodule-loading in a client and look for the diagnostics output.
// RUN: %target-swift-frontend -typecheck %t/Client.swift -Rmodule-loading \
// RUN:   -I %t -module-cache-path %t/cache 2>&1 | %FileCheck %s

//--- module.modulemap
module IndirectMixedDependency {
  header "IndirectMixedDependency.h"
}

module DirectMixedDependency {
  header "DirectMixedDependency.h"
  export *
}

//--- IndirectMixedDependency.h

//--- IndirectMixedDependency.swift

@_exported import IndirectMixedDependency

//--- DirectMixedDependency.h

#include "IndirectMixedDependency.h"

//--- DirectMixedDependency.swift

@_exported import DirectMixedDependency

//--- SwiftDependency.swift

public func publicFunction() {}

//--- Client.swift

import SwiftDependency
import SwiftNonResilientDependency
import DirectMixedDependency

// CHECK: remark: 'Swift' has a required transitive dependency on 'SwiftShims'
// CHECK: remark: loaded module 'SwiftShims'; source: '{{.*}}module.modulemap', loaded: '{{.*}}SwiftShims-{{.*}}.pcm'
// CHECK: remark: loaded module 'Swift'; source: '{{.*}}Swift.swiftmodule{{.*}}.swiftinterface', loaded: '{{.*}}Swift.swiftmodule{{.*}}.swiftmodule'
// CHECK: remark: 'SwiftDependency' has a required transitive dependency on 'Swift'
// CHECK: remark: loaded module 'SwiftDependency'; source: '{{.*}}SwiftDependency.swiftinterface', loaded: '{{.*}}SwiftDependency-{{.*}}.swiftmodule'
// CHECK-DAG: remark: loaded module 'SwiftNonResilientDependency'; source: '{{.*}}SwiftNonResilientDependency.swiftmodule', loaded: '{{.*}}SwiftNonResilientDependency.swiftmodule'
// CHECK-DAG: remark: loaded module 'IndirectMixedDependency' (overlay for a clang dependency); source: '{{.*}}IndirectMixedDependency.swiftinterface', loaded: '{{.*}}IndirectMixedDependency-{{.*}}.swiftmodule'
// CHECK-DAG: remark: loaded module 'DirectMixedDependency'; source: '{{.*}}module.modulemap', loaded: '{{.*}}DirectMixedDependency-{{.*}}.pcm'
