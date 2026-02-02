import SwiftUI

public struct OnOffColor {
    public var on: any ShapeStyle { fatalError() }

    public func resolve() -> some ShapeStyle {
        AnyShapeStyle(on)
    }
}

extension View {
    @inlinable public func foregroundStyle(_ style: OnOffColor, isOn: Bool) -> some View {
        foregroundStyle(style.resolve())
    }
}
