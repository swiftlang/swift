// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -solver-scope-threshold=10000

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import Foundation
import SwiftUI

struct MyView: View {
    public enum Style {
        case focusRing(platterSize: CGSize, stroke: CGFloat, offset: CGFloat)
     }

    var style: Style
    var isFocused: Bool
    var focusColor: Color

    var body: some View {
        Group {
            switch style {
            case let .focusRing(platterSize: platterSize, stroke: focusRingStroke, offset: focusRingOffset):
                Circle()
                  .overlay {
                      Circle()
                        .stroke(isFocused ? focusColor : Color.clear, lineWidth: focusRingStroke)
                        .frame(
                          width: platterSize.width + (2 * focusRingOffset) + focusRingStroke,
                          height: platterSize.height + (2 * focusRingOffset) + focusRingStroke
                        )
                  }
            }
        }
    }
}
