// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/isolated_conformance_other.swiftmodule %S/Inputs/isolated_conformance_other.swift -swift-version 6
// RUN: %target-typecheck-verify-swift -I %t -swift-version 6 -enable-upcoming-feature InferIsolatedConformances -default-isolation MainActor -swift-version 6
// REQUIRES: swift_feature_InferIsolatedConformances

import isolated_conformance_other

struct S1: P { func f() {} }
struct S2: PDerived { func f() {} }
