// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan
// REQUIRES: rdar36854536

class Color {
  init(hue: Double, saturation: Double, brightness: Double, alpha: Double) {}
  init(red: Double, green: Double, blue: Double, alpha: Double) {}
}

// expected-error@+1 {{reasonable time}}
let _: [Color] = [
    Color(r: 3, g: 72, b: 14, a: 48),
    Color(r: 0, g: 240, b: 64, a: 77),
    Color(r: 0, g: 255, b: 0, a: 160),
    Color(r: 0, g: 168, b: 0, a: 255),
    Color(r: 0, g: 140, b: 0, a: 255),
    Color(r: 0, g: 112, b: 0, a: 255),
    Color(r: 255, g: 255, b: 0, a: 255),
    Color(r: 184, g: 184, b: 0, a: 255),
    Color(r: 224, g: 112, b: 0, a: 255),
    Color(r: 255, g: 0, b: 0, a: 255),
    Color(r: 184, g: 0, b: 0, a: 255),
    Color(r: 112, g: 0, b: 0, a: 255),
    Color(r: 255, g: 0, b: 255, a: 255),
    Color(r: 255, g: 0, b: 255, a: 255),
    Color(r: 255, g: 0, b: 255, a: 255),
    Color(r: 0, g: 0, b: 0, a: 0),
]
