// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15
// REQUIRES: OS=macosx

import SwiftUI

struct TestView: View {
  @State var angle: Angle = Angle(radians: Double.pi/1.5)
  let size = CGSize(width: 800, height: 600)
  var body: some View {
    Path { path in
      path.move(to: CGPoint(x: size.width/cos(angle.radians),
                            y: size.height/sin(angle.radians)))
      path.addLine(to: CGPoint(x: 100, y: 100))
    }.stroke(Color.red)
  }
}
