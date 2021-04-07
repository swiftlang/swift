// RUN: %target-swift-emit-silgen -module-name main %s -verify | %FileCheck %s
// SR-9425
enum MyState : String {
    case closed = "closed"
    case opened = "opened"
}

@inline(never)
func check_state(_ state : MyState) -> Int {
  // CHECK: function_ref @$s4main7MyStateO21__derived_enum_equalsySbAC_ACtFZ
  return state == .opened ? 1 : 0
}

// generic-enum.swift
enum GenericMyState<T> : String {
  case closed
  case opened
}

@inline(never)
func check_generic_state(_ state : GenericMyState<Int>) -> Int {
  // CHECK: function_ref @$s4main14GenericMyStateO21__derived_enum_equalsySbACyxG_AEtFZ
  return state == .opened ? 1 : 0
}

// regular-enum.swift
enum Regular {
  case closed
  case opened
}

@inline(never)
func check_regular(_ state : Regular) -> Int {
  // CHECK: function_ref @$s4main7RegularO21__derived_enum_equalsySbAC_ACtFZ
  return state == .closed ? 1 : 0
}

// string-enum.swift
enum Alphabet : String {
  case A = "A", B = "B", C = "C", D = "D", E = "E", F = "F", G = "G", H = "H", I = "I", J = "J"
}

@inline(never)
func check_alphabet(_ state : Alphabet) -> Int {
  // CHECK: function_ref @$s4main8AlphabetO21__derived_enum_equalsySbAC_ACtFZ
  return state == .E ? 1 : 0
}

@inline(never)
func compareIt(_ state : Alphabet, _ rhs: Alphabet) -> Bool {
  // CHECK: function_ref @$s4main8AlphabetO21__derived_enum_equalsySbAC_ACtFZ
  return state == rhs
}

// int-enum.swift
enum AlphabetInt : Int {
  case A = 10, B = 100, C = 12, D = 456, E = 1, F = 3, G = 77, H = 2, I = 27, J = 42
}

@inline(never)
func check_alphabet_int(_ state : AlphabetInt) -> Int {
  // CHECK: function_ref @$s4main11AlphabetIntO21__derived_enum_equalsySbAC_ACtFZ
  return state == .E ? 1 : 0
}

@inline(never)
func compareIt(_ state : AlphabetInt, _ rhs: AlphabetInt) -> Bool {
  // CHECK: function_ref @$s4main11AlphabetIntO21__derived_enum_equalsySbAC_ACtFZ
  return state == rhs
}
