// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -O -cross-module-optimization -sil-verify-all

// REQUIRES: OS=macosx

import SwiftUI

public struct Stroke {}

public struct AnyInsettableShape: InsettableShape {
  public func path(in rect: CGRect) -> Path { Path() }
  public func inset(by amount: CGFloat) -> some InsettableShape { self }
}

private struct StrokeModifier<S>: ViewModifier {
  var stroke: Stroke
  var shape: S
  func body(content: Content) -> some View {}
}

extension View {
  public func stroke(_ stroke: Stroke, shape: InsettableShape) -> some View {
    modifier(StrokeModifier(stroke: stroke, shape: shape))
  }
}
