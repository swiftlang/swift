// RUN: %target-typecheck-verify-swift

typealias rgb = Int32 // expected-note {{declared here}}
var rgb : rgb? // expected-error {{invalid redeclaration of 'rgb'}}

// This used to produce a diagnostic about 'rgba' being used in its own
// type, but arguably that is incorrect, since we are referencing a
// different 'rgba'.
struct Color {
    var rgba : rgba? {
        return nil
    }

    typealias rgba = Int32
}

struct Color2 {
    let rgba : rgba?

    struct rgba {}
}

typealias Integer = Int

var i: Integer

struct Hair<Style> {
  typealias Hairdo = Style
  typealias MorningHair = Style?

  func fancy() -> Hairdo {}
  func wakeUp() -> MorningHair {}
}

typealias FunnyHair = Hair<Color>

var f: FunnyHair
