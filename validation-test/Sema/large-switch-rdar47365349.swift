// RUN: %target-typecheck-verify-swift

enum NumericBase {
  case binary
  case ternary
  case quaternary
  case quinary
  case senary
  case septary
  case octal
  case nonary
  case decimal
  case undecimal
  case duodecimal
}

enum Direction {
  case left
  case right
}

enum WritingSystem {
  case logographic
  case alphabet(kind: Alphabet)
  case abjad
  case abugida
  case syllabary
  case other
}

enum Alphabet {
  case roman
  case greek
  case cyrillic
}

func test(base: NumericBase, direction: Direction, writingSystem: WritingSystem) {
  switch (base, direction, writingSystem) {
    case (.binary, .left, .logographic),
      (.binary, .left, .alphabet),
      (.binary, .left, .abugida):
      break
      
    case (.binary, .right, .logographic),
      (.binary, .right, .alphabet),
      (.binary, .right, .abugida):
      break
      
    case (.binary, _, .abjad):
      break
      
    case (.binary, _, .syllabary):
      break
      
    case (.ternary, .left, .logographic):
      break
      
    case (.ternary, .left, .alphabet),
      (.ternary, .left, .abugida):
      break
      
    case (.ternary, .right, .logographic),
      (.ternary, .right, .abugida):
      break
      
    case (.ternary, .right, .alphabet):
      break
      
    case (.ternary, _, .abjad):
      break
      
    case (.ternary, _, .syllabary):
      break
      
    case (.quaternary, .left, .logographic):
      break
      
    case (.quaternary, .left, .alphabet),
      (.quaternary, .left, .abugida):
      break
      
    case (.quaternary, .right, .logographic),
      (.quaternary, .right, .abugida):
      break
      
    case (.quaternary, .right, .alphabet):
      break
      
    case (.quaternary, _, .abjad):
      break
      
    case (.quaternary, _, .syllabary):
      break
    
    case (.quinary, .left, .logographic),
      (.senary, .left, .logographic):
      break
      
    case (.quinary, .left, .alphabet),
      (.senary, .left, .alphabet),
      (.quinary, .left, .abugida),
      (.senary, .left, .abugida):
      break
      
    case (.quinary, .right, .logographic),
      (.senary, .right, .logographic):
      break
      
    case (.quinary, .right, .alphabet),
      (.senary, .right, .alphabet),
      (.quinary, .right, .abugida),
      (.senary, .right, .abugida):
      break
      
    case (.quinary, _, .abjad),
      (.senary, _, .abjad):
      break
      
    case (.quinary, _, .syllabary),
      (.senary, _, .syllabary):
      break
      
    case (.septary, .left, .logographic):
      break
      
    case (.septary, .left, .alphabet),
      (.septary, .left, .abugida):
      break
      
    case (.septary, .right, .logographic):
      break
      
    case (.septary, .right, .alphabet),
      (.septary, .right, .abugida):
      break
      
    case (.septary, _, .abjad):
      break
      
    case (.septary, _, .syllabary):
      break
      
    case (.decimal, .left, .logographic):
      break
      
    case (.decimal, .left, .alphabet),
      (.decimal, .left, .abugida):
      break
      
    case (.decimal, .right, .logographic):
      break
      
    case (.decimal, .right, .alphabet),
      (.decimal, .right, .abugida):
      break
      
    case (.octal, .left, .logographic),
      (.nonary, .left, .logographic):
      break
      
    case (.octal, .left, .alphabet),
      (.nonary, .left, .alphabet),
      (.octal, .left, .abugida),
      (.nonary, .left, .abugida):
      break
      
    case (.octal, .right, .logographic),
      (.nonary, .right, .logographic):
      break
      
    case (.octal, .right, .alphabet),
      (.nonary, .right, .alphabet),
      (.octal, .right, .abugida),
      (.nonary, .right, .abugida):
      break
      
    case (.octal, _, .abjad),
      (.nonary, _, .abjad),
      (.decimal, _, .abjad):
      break
      
    case (.octal, _, .syllabary),
      (.nonary, _, .syllabary),
      (.decimal, _, .syllabary):
      break
      
    case (.undecimal, .left, .logographic):
      break
      
    case (.undecimal, .left, .alphabet),
      (.undecimal, .left, .abugida):
      break
      
    case (.undecimal, .right, .logographic):
      break
      
    case (.undecimal, .right, .alphabet),
      (.undecimal, .right, .abugida):
      break
      
    case (.undecimal, _, .abjad):
      break
      
    case (.undecimal, _, .syllabary):
      break
      
    case (.duodecimal, _, _):
      break
    case (_, _, .other):
      break
  }
}
