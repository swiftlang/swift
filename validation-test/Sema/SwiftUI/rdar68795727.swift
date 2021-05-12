// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct UnitVolumeView: View {
  var measurement: Measurement<Unit>

  var body: some View {
    Text("")
  }
}
struct UnitView_Previews: PreviewProvider {
  static var previews: some View {
    let volume: Measurement<Unit> = Measurement<UnitVolume>(value: 200, unit: UnitVolume.milliliters)
    // expected-error@-1 {{cannot assign value of type 'Measurement<UnitVolume>' to type 'Measurement<Unit>'}}
    // expected-note@-2 {{arguments to generic parameter 'UnitType' ('UnitVolume' and 'Unit') are expected to be equal}}

    Group {
      ForEach(["en", "de", "fr"], id: \.self) { id in
        UnitVolumeView(measurement: volume)
          .previewLayout(PreviewLayout.sizeThatFits)
          .environment(\.locale, .init(identifier: id))
      }
    }
  }
}
