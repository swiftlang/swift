// RUN: %target-parse-verify-swift

typealias rgb = Int32 // expected-note {{declared here}}
var rgb : rgb? // expected-error {{invalid redeclaration of 'rgb'}}

struct Color {
    var rgba : rgba? { // expected-error {{'rgba' used within its own type}}
        return nil
    }

    typealias rgba = Int32
}

struct Color2 {
    let rgba : rgba? // expected-error {{'rgba' used within its own type}}

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
