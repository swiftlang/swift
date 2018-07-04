// RUN: %target-typecheck-verify-swift -D ENABLE_C

enum E {
  case A,B
#if ENABLE_C
  case C
#endif
#if ENABLE_D
  case D
#endif
}

func foo(x: E, intVal: Int) {
  // Active guarded case first.
  switch x {
#if ENABLE_C
    case .C:
      break
#endif
    case .A:
      break
    case .B:
      break
  }

  // Active guarded case last.
  switch x {
    case .A:
      break
    case .B:
      break
#if ENABLE_C
    case .C:
      break
#endif
  }

  // Active guarded case middle.
  switch x {
    case .A:
      break
#if ENABLE_C
    case .C:
      break
#endif
    default:
      break
  }

  // Active guarded case after default.
  switch x {
    case .A:
      break
    default:
      break
#if ENABLE_C
    case .C: // expected-error {{additional 'case' blocks cannot appear after the 'default' block of a 'switch'}}
      break
#endif
  }

  // Inactive guarded case after default.
  switch x {
    case .A:
      break
    default:
      break
#if ENABLE_D
    case .D:
      break
#endif
  }

  // #elseif.
  switch x {
    case .A:
      break
    case .B:
      break
#if NEVER
#elseif ENABLE_C
    case .C:
      break
#endif
  }

  // #else.
  switch x {
    case .A:
      break
    case .B:
      break
#if !ENABLE_C
#else
    case .C:
      break
#endif
  }

  // Nested #if.
  switch x {
    case .A:
      break
    case .B:
      break
#if ENABLE_C
  #if NEVER
  #else
    case .C:
      break
  #endif
#endif
  }

  // Exhaustive check.
  switch x { // expected-error {{switch must be exhaustive}} expected-note {{add missing case: '.C'}}
    case .A:
      break
    case .B:
      break
#if NEVER
    case .C:
      break
#endif
  }

  // Exhaustive check 2.
  switch x {
#if ENABLE_C
    case .A:
      break
    case .B:
      break
    case .C:
      break
#endif
  }

  // Empty check.
  switch intVal { // expected-error {{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}
#if NEVER
    case 1:
      break
    case 2:
      break
    case 3:
      break
#endif
  }

  // Non-'case' statement in '#else' block.
  switch x {
    case .A:
      break
    case .B:
      break
#if ENABLE_C
    case .C:
      break
#else
      break // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
#endif
  }

  // Non-'case' statement in '#if' block.
  switch x { // expected-error {{switch must be exhaustive}} expected-note {{add missing case: '.C'}}
    case .A:
      break
    case .B:
      break
#if !ENABLE_C
      break
#else
    case .C: // expected-error {{'case' label can only appear inside a 'switch' statement}}
      break
#endif
  }

  // '#if ... case' after non-covered statements.
  switch x {
    print() // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
#if ENABLE_C
    case .NOT_EXIST: // expected-error {{pattern cannot match values of type 'E'}}
      break
    case .C:
      break
#endif
    case .A, .B:
      break
  }

  // '#if ... stmt' after non-covered statements.
  switch x {
    print() // expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}}
#if true
    print()
#endif
#if ENABLE_C
    case .C:
      break
#endif
    case .A, .B:
      break
  }

  // 'fallthrough' target.
  switch intVal {
    case 1:
      fallthrough // expected-error {{'fallthrough' from a case which doesn't bind variable 'val'}}
#if ENABLE_C
    case let val:
      break
#endif
    case 2:
      fallthrough // expected-error {{'fallthrough' without a following 'case' or 'default' block}}
#if NEVER
    case 3:
      break
#endif
  }
}
