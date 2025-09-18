// RUN: %empty-directory(%t)
// RUN: mkdir %t/a %t/b
// RUN: %target-swift-frontend -emit-module -o %t/b/B.swiftmodule %S/../Inputs/empty.swift -disable-implicit-swift-modules -parse-stdlib
//-no-serialize-debugging-options
// RUN: %target-swift-frontend -emit-module -o %t/a/A.swiftmodule %S/Inputs/a.swift -swift-module-file=B=%t/b/B.swiftmodule -disable-implicit-swift-modules -parse-stdlib

// Not passing in path to b/B.swiftmodule -- it should be found in the INPUT block.
// RUN: %target-swift-frontend -emit-module -o %t/Library.swiftmodule %s -swift-module-file=A=%t/a/A.swiftmodule -disable-implicit-swift-modules -parse-stdlib
import A
