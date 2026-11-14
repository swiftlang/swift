// RUN: %target-swift-frontend -typecheck %s -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple -dump-requirement-machine 2>&1 | %FileCheck --check-prefixes=COMMON,NOFEATURE %s
// RUN: %target-swift-frontend -typecheck %s -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -target %target-swift-5.8-abi-triple -enable-experimental-feature AnyReference -dump-requirement-machine 2>&1 | %FileCheck --check-prefixes=COMMON,FEATURE %s

// REQUIRES: swift_feature_AnyReference

import AnyReferenceFRTs

struct Generic<T> {}
extension Generic where T == RefCountedType {}

// COMMON: τ_0_0.[concrete: RefCountedType] => τ_0_0 [explicit]
// FEATURE: τ_0_0.[layout: _AnyReference] => τ_0_0
// NOFEATURE-NOT: layout:
