// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DocumentationMetadata -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DocumentationMetadata -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DocumentationMetadata.symbols.json

// CHECK-DAG: "metadata": "cool_stuff"
@_documentation(metadata: cool_stuff) public class SomeClass {}

// CHECK-DAG: "metadata": "this is a longer string"
@_documentation(metadata: "this is a longer string") public class OtherClass {}

