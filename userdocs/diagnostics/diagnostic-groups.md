# Diagnostic groups

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Detailed explanations for various compiler diagnostics.


## Overview

Diagnostic groups collect some number of diagnostics together under a common group name. This allows
for extra documentation to help explain relevant language concepts, as well as the ability to
control the behavior of warnings in a more precise manner (when that group contains warnings):
- `-Werror <group>` - upgrades warnings in the specified group to errors
- `-Wwarning <group>` - indicates that warnings in the specified group should remain warnings, even
  if they were previously upgraded to errors

As a concrete example, to upgrade deprecated declaration warnings to errors:
```sh
-Werror DeprecatedDeclaration
```

Or upgrade all warnings except deprecated declaration to errors:
```sh
-warnings-as-errors -Wwarning DeprecatedDeclaration
```

## Groups with warnings
- <doc:always-available-domain>
- <doc:trailing-closure-matching>
- <doc:compilation-caching>
- <doc:string-interpolation-conformance>
- <doc:deprecated-declaration>
- <doc:implementation-only-deprecated>
- <doc:dynamic-exclusivity>
- <doc:embedded-restrictions>
- <doc:explicit-sendable-annotations>
- <doc:preconcurrency-import>
- <doc:foreign-reference-type>
- <doc:clang-declaration-import>
- <doc:isolated-conformances>
- <doc:error-in-future-swift-version>
- <doc:module-version-missing>
- <doc:result-builder-methods>
- <doc:semantic-copies>
- <doc:strict-language-features>
- <doc:strict-memory-safety>
- <doc:unknown-warning-group>


## Topics
- <doc:dynamic-callable-requirements>
- <doc:always-available-domain>
- <doc:trailing-closure-matching>
- <doc:actor-isolated-call>
- <doc:sendable-closure-captures>
- <doc:compilation-caching>
- <doc:string-interpolation-conformance>
- <doc:deprecated-declaration>
- <doc:implementation-only-deprecated>
- <doc:dynamic-exclusivity>
- <doc:embedded-restrictions>
- <doc:explicit-sendable-annotations>
- <doc:preconcurrency-import>
- <doc:foreign-reference-type>
- <doc:clang-declaration-import>
- <doc:isolated-conformances>
- <doc:error-in-future-swift-version>
- <doc:missing-module-on-known-paths>
- <doc:module-version-missing>
- <doc:module-not-testable>
- <doc:multiple-inheritance>
- <doc:nominal-types>
- <doc:exclusivity-violation>
- <doc:performance-hints>
- <doc:property-wrapper-requirements>
- <doc:conformance-isolation>
- <doc:protocol-type-non-conformance>
- <doc:result-builder-methods>
- <doc:semantic-copies>
- <doc:sendable-metatypes>
- <doc:sending-closure-risks-data-race>
- <doc:sending-risks-data-race>
- <doc:strict-language-features>
- <doc:strict-memory-safety>
- <doc:temporary-pointers>
- <doc:opaque-type-inference>
- <doc:unknown-warning-group>
- <doc:availability-unrecognized-name>
- <doc:mutable-global-variable>
- <doc:existential-member-access-limitations>
