# Diagnostic groups

<!-- This file is auto-generated via `swift swift/utils/generate-doc-index.swift` -->

Diagnostic groups allow controlling the behavior of warnings in a more precise manner.


## Overview

Diagnostic groups collect some number of diagnostics together under a common group name. This allows
for extra documentation to help explain relevant language concepts, as well as the ability to
control the behavior of warnings in a more precise manner:
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


## Topics

- <doc:compilation-caching>
- <doc:deprecated-declaration>
- <doc:implementation-only-deprecated>
- <doc:preconcurrency-import>
- <doc:clang-declaration-import>
- <doc:missing-module-on-known-paths>
- <doc:module-version-missing>
- <doc:strict-language-features>
- <doc:strict-memory-safety>
- <doc:unknown-warning-group>
- <doc:availability-unrecognized-name>
