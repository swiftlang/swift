// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-library-evolution \
// RUN:   -emit-module-path=%t/lib.swiftmodule -module-name=lib \
// RUN:   -validate-tbd-against-ir=none \
// RUN:   %S/Inputs/force_public_metadata_accessors.swift
// RUN: %target-swift-frontend -parse-as-library -enable-library-evolution \
// RUN:   -force-public-linkage \
// RUN:   -validate-tbd-against-ir=none -emit-ir %s -I %t | %FileCheck %s

import lib

private enum FixedContainer {
  case a(S)
}

fileprivate var c = FixedContainer.a(S())
public func use() -> Int {
  switch (c) {
  case let .a(s):
    return s.a
  }
}

// CHECK: define {{.*}} @"$s31force_public_metadata_accessors3useSiyF"()
// CHECK-NOT: define
// CHECK: call {{.*}} %swift.metadata_response @"$s31force_public_metadata_accessors14FixedContainer{{.*}}LLOMa"

// FIXME: From within LLDB, this would be a forward declaration.
// Unfortunately this is difficult to reproduce from source alone.
// Really this should be a check for a non-internal "declare".
// CHECK: define{{.*}} swiftcc %swift.metadata_response @"$s31force_public_metadata_accessors14FixedContainer{{.*}}LLOMa"


