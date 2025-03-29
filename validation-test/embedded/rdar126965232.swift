// RUN: %target-swiftc_driver                     \
// RUN:     -c                                    \
// RUN:     %s                                    \
// RUN:     -Xfrontend -sil-verify-all            \
// RUN:     -enable-experimental-feature Embedded \
// RUN:     -wmo                                  \
// RUN:     -Osize                                \
// RUN:     -o %t/bin

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

class MyClass {
  var x: Int? = nil
  var enabled: Bool = true
}

public func app_main() {
  let object = MyClass()
  while true {
    let enabled = object.enabled
    object.enabled = !enabled
  }
}
