// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ObservableUsableFromInline)) -enable-library-evolution -Xfrontend -plugin-path -Xfrontend %swift-plugin-dir %S/Inputs/ObservableUsableFromInline.swift -emit-module -emit-module-path %t/ObservableUsableFromInline.swiftmodule -module-name ObservableUsableFromInline
// RUN: %target-codesign %t/%target-library-name(ObservableUsableFromInline)
// RUN: %target-build-swift -parse-as-library %s -lObservableUsableFromInline -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(ObservableUsableFromInline)

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Observation
import ObservableUsableFromInline

@main
struct Validator {
  static func main() {
    let suite = TestSuite("ObservableUsableFromInline")

    suite.test("inlinable access is observed") {
      guard #available(SwiftStdlib 5.9, *) else { return }
      let object = ObservableUsableFromInline(value: 0)
      var changed = 0
      withObservationTracking {
        _blackHole(object.get())
      } onChange: {
        changed += 1
      }
      expectEqual(changed, 0)
      object.set(1)
      expectEqual(changed, 1)
      expectEqual(object.get(), 1)
    }

    runAllTests()
  }
}
