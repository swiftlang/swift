// RUN: %target-swift-frontend %s -module-name Application -parse-as-library -entry-point-function-name Application_main -mergeable-symbols    -enable-experimental-feature Embedded -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend %s -module-name Application -parse-as-library -entry-point-function-name Application_main -mergeable-symbols -O -enable-experimental-feature Embedded -emit-ir | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    print("hello")
  }
}

// CHECK: @Application_main
