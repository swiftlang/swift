// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -solver-scope-threshold=1000
// REQUIRES: OS=macosx

import SwiftUI

func test(a: [(offset: Int, element: Double)],
          c: Color,
          x: CGFloat,
          n: Int
) -> some View {
  ForEach(a, id: \.offset) { i, r in
    RoundedRectangle(cornerRadius: r, style: .continuous)
      .stroke(c, lineWidth: 1)
      .padding(.horizontal, x / Double(n) * Double(n - 1 - i) / 2)
      .padding(.vertical, x / Double(n) * Double(n - 1 - i) / 2)
  }
}
