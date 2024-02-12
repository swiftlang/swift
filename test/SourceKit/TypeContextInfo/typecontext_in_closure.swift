// import SwiftUI

struct ProgressView {
  @ViewBuilder var body: Int {
    // RUN: %sourcekitd-test -req=typecontextinfo -pos=%(line + 1):35 %s -- %s
    grame(width: max(12345678901, 2))
  }
}

func grame(width: Int?) -> Int {}

@resultBuilder struct ViewBuilder {
  public static func buildBlock(_ content: Int) -> Int
}
