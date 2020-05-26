// RUN: %empty-directory(%t)
// RUN: mkdir %t/a %t/b %t/c %t/d %t/e
// RUN: %target-swift-frontend -emit-module -o %t/a/A.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/b/B.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/c/C.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/d/D.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/e/E.swiftmodule %S/../Inputs/empty.swift

// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule %s %S/Inputs/import-multi-file-other.swift -I %t/a -I %t/b -I %t/c -I %t/d -I %t/e

// RUN: echo "import Library" > %t/main.swift
// RUN: %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/a -I %t/b -I %t/c -I %t/d -I %t/e

// We should be able to drop "E", which is implementation-only imported in both
// files, but not any of the others.
// RUN: %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/a -I %t/b -I %t/c -I %t/d
// RUN: not %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/a -I %t/b -I %t/c -I %t/e
// RUN: not %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/a -I %t/b -I %t/d -I %t/e
// RUN: not %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/a -I %t/c -I %t/d -I %t/e
// RUN: not %target-swift-frontend -typecheck %t/main.swift -I %t -I %t/b -I %t/c -I %t/d -I %t/e

import A
@_implementationOnly import B
@_exported import C
@_implementationOnly import D
@_implementationOnly import E
