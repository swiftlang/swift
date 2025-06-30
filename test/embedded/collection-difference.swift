// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

@main
struct Main {
  static func main() {
    let a = [1, 2, 3, 4, 5]
    let b = [   2, 3, 4,    6]
    let diff = b.difference(from: a)
    for entry in diff {
        switch entry {
        case .remove(let offset, let element, _): print(".remove(\(offset), \(element))")
        case .insert(let offset, let element, _): print(".insert(\(offset), \(element))")
        }
    }
    // CHECK: .remove(4, 5)
    // CHECK: .remove(0, 1)
    // CHECK: .insert(3, 6)
  }
}
