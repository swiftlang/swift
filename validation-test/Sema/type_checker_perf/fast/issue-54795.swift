// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// Succeeds in 60ms with 5572 scopes

// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/54795

import SwiftUI

struct Breathe: View {
    static private let colors: [Color] = [Color(#colorLiteral(red: 0.3529411765, green: 0.662745098, blue: 0.6745098039, alpha: 1)), Color(#colorLiteral(red: 0.4078431373, green: 0.7450980392, blue: 0.6549019608, alpha: 1)), Color(#colorLiteral(red: 0.4666666667, green: 0.8196078431, blue: 0.631372549, alpha: 1)), Color(#colorLiteral(red: 0.5179253817, green: 0.8318992257, blue: 0.6992306113, alpha: 1)), Color(#colorLiteral(red: 0.4078431373, green: 0.7333333333, blue: 0.6509803922, alpha: 1)), Color(#colorLiteral(red: 0.3568627451, green: 0.6666666667, blue: 0.6745098039, alpha: 1))]

    @State private var leafSize: CGFloat = 50

    var body: some View {
        ZStack {
            ForEach(0..<Self.colors.count, id: \.self) { index in
                Circle()
                    .fill(Self.colors[index])
                    .opacity(0.5)
                    .offset(x: -self.leafSize / 2)
                    .rotationEffect(.degrees(Double(index * 360 / Self.colors.count)))
            }
        }
        .frame(width: self.leafSize, height: self.leafSize)   // <== The compiler is unable to type-check this expression in reasonable time
    }
}
