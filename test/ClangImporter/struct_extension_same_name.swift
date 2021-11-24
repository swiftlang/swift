// RUN: %target-swift-frontend -c %clang-importer-sdk -enable-objc-interop %s

import CoreGraphics

extension CGSize {
    public static func width(_ value: CGFloat) -> CGSize {
        return CGSize(width: value, height: 0)
    }
}
