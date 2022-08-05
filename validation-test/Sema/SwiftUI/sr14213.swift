// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct Experiment: View {
  var body: some View {
    HStack {
      Slider(value: <#T##Binding<BinaryFloatingPoint>#>, in: <#T##ClosedRange<BinaryFloatingPoint>#>, label: <#T##() -> _#>) // expected-error 3 {{editor placeholder in source file}}
      // expected-error@-1 {{type 'any BinaryFloatingPoint' cannot conform to 'Comparable'}}
      // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
      // expected-error@-3 {{type 'any BinaryFloatingPoint' cannot conform to 'BinaryFloatingPoint'}}
      // expected-note@-4 {{only concrete types such as structs, enums and classes can conform to protocols}}
      // expected-note@-5 {{required by initializer 'init(value:in:label:onEditingChanged:)' where 'V' = 'any BinaryFloatingPoint'}}
    }
  }
}
