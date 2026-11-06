// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name main -emit-silgen %s | %FileCheck %s --check-prefix=HOSTED
// RUN: %if OS=macosx && CODEGENERATOR=ARM && embedded_stdlib_cross_compiling %{ %target-swift-frontend -target armv7em-none-none-eabi -enable-experimental-feature Embedded -parse-as-library -module-name main -emit-silgen %s | %FileCheck %s --check-prefix=BAREMETAL %}

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

@main
struct Main {
  // HOSTED-LABEL: // static Main.main()
  // HOSTED-NEXT: // Isolation: global_actor. type: MainActor
  // BAREMETAL-LABEL: // static Main.main()
  // BAREMETAL-NEXT: // Isolation: unspecified
  static func main() async {
    print("main")
  }
}
