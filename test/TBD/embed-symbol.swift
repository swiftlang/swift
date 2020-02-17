// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: echo 'public class Foo {}' > %t/foo.swift
// RUN: echo 'public class Bar {}' > %t/bar.swift
// RUN: %target-swift-frontend -emit-module %t/foo.swift -emit-module-path %t/foo.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/bar.swift -emit-module-path %t/bar.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/flag-not-provided.tbd -I %t -module-name main
// RUN: %FileCheck %s --check-prefix FLAG-NOT-PROVIDED < %t/flag-not-provided.tbd

// RUN: %target-swift-frontend -typecheck %s -emit-tbd -emit-tbd-path %t/flag-provided.tbd -I %t -embed-tbd-for-module foo -embed-tbd-for-module bar -module-name main
// RUN: %FileCheck %s --check-prefix FLAG-PROVIDED < %t/flag-provided.tbd

import foo
import bar

// FLAG-NOT-PROVIDED-NOT: $s3bar3BarCMa
// FLAG-NOT-PROVIDED-NOT: $s3foo3FooCMa

// FLAG-PROVIDED: $s3bar3BarCMa
// FLAG-PROVIDED: $s3foo3FooCMa
