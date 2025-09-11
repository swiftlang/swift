RUN: %sourcekitd-test -req=interface-gen -module Foundation -- -target %target-triple -sdk %sdk | %FileCheck %s
RUN: %sourcekitd-test -req=interface-gen -module Foundation.NSArray -- -target %target-triple -sdk %sdk | %FileCheck %s --check-prefix=CHECK-NSARRAY

REQUIRES: OS=macosx

This tests that printing the interface of a submodule does not print imports of other submodules of the same TLM.

CHECK: import Foundation.NSArray
CHECK-NSARRAY-NOT: import
