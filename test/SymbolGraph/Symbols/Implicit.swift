// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Implicit -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Implicit -I %t -pretty-print -output-dir %t -minimum-access-level internal
// RUN: %FileCheck %s --input-file %t/Implicit.symbols.json

// RUN: %target-swift-symbolgraph-extract -module-name Implicit -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Implicit.symbols.json --check-prefix PUBLIC

public class SomeClass {}

public struct SomeStruct {
    let bar: Int
    let other: String
}

// make sure that the implicitly-generated initializer appears when the access level is `internal`
// CHECK-DAG: "precise": "s:8Implicit9SomeClassCACycfc"
// CHECK-DAG: "precise": "s:8Implicit10SomeStructV3bar5otherACSi_SStcfc"

// ...but that they don't show up when it's `public`, since they're marked `internal` to begin with
// PUBLIC-NOT: "precise": "s:8Implicit9SomeClassCACycfc"
// PUBLIC-NOT: "precise": "s:8Implicit10SomeStructV3bar5otherACSi_SStcfc"
