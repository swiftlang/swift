# Customizing Your C Code for Swift

Use the `CF_SWIFT_NAME` macro to group functions that have related behavior.

## Overview

Because structures in C can't have methods, property accessors, or custom initializers,
you often need to write such functionality using global functions. Structures in
Swift can declare methods, property accessors, and initializers. You use the `CF_SWIFT_NAME`
macro to group together related global functions into a single structure type that's
imported into Swift.

### Apply CF_SWIFT_NAME to Related Functions

The example below shows several functions that are all related to a `Color` type.
The `CF_SWIFT_NAME` macro is applied to each function, giving each one a new name
for Swift that's nested together under the `Color` type:

```occ
Color ColorCreateWithCMYK(float c, float m, float y, float k) CF_SWIFT_NAME(Color.init(c:m:y:k:));

float ColorGetHue(Color color) CF_SWIFT_NAME(getter:Color.hue(self:));
void ColorSetHue(Color color, float hue) CF_SWIFT_NAME(setter:Color.hue(self:newValue:));

Color ColorDarkenColor(Color color, float amount) CF_SWIFT_NAME(Color.darken(self:amount:));

extern const Color ColorBondiBlue CF_SWIFT_NAME(Color.bondiBlue);

Color ColorGetCalibrationColor(void) CF_SWIFT_NAME(getter:Color.calibration());
Color ColorSetCalibrationColor(Color color) CF_SWIFT_NAME(setter:Color.calibration(newValue:));
```

The argument you pass to the `CF_SWIFT_NAME` macro uses the same syntax as the `#selector`
expression. You use `self` in a `CF_SWIFT_NAME` argument to refer to the instance
that the method belongs to.

### Import Related Functions into Swift

Here's how Swift imports the related functions above into a single type:

```swift
extension Color {
    init(c: Float, m: Float, y: Float, k: Float)

    var hue: Float { get set }

    func darken(amount: Float) -> Color

    static var bondiBlue: Color

    static var calibration: Color
}
```

> Note: You can't reorder or change the number of arguments for type members imported
using the `CF_SWIFT_NAME` macro.
