// RUN: %target-swift-emit-silgen -module-name main %s -verify | %FileCheck %s --check-prefix=FRAGILE
// RUN: %target-swift-emit-silgen -module-name main %s -verify -enable-library-evolution | %FileCheck %s --check-prefix=RESILIENT

// SR-9425
public enum MyState : String {
    case closed = "closed"
    case opened = "opened"
}

// CHECK-LABEL: sil [ossa] @$s4main11check_stateySiAA7MyStateOF : $@convention(thin) (MyState) -> Int {
public func check_state(_ state : MyState) -> Int {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == .opened ? 1 : 0
}

// generic-enum.swift
public enum GenericMyState<T> : String {
  case closed
  case opened
}

// CHECK-LABEL: sil [ossa] @$s4main19check_generic_stateySiAA14GenericMyStateOySiGF : $@convention(thin) (GenericMyState<Int>) -> Int {
public func check_generic_state(_ state : GenericMyState<Int>) -> Int {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == .opened ? 1 : 0
}

// regular-enum.swift
public enum Regular {
  case closed
  case opened
}

// CHECK-LABEL: sil [ossa] @$s4main13check_regularySiAA7RegularOF : $@convention(thin) (Regular) -> Int {
public func check_regular(_ state : Regular) -> Int {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == .closed ? 1 : 0
}

// string-enum.swift
public enum Alphabet : String {
  case A = "A", B = "B", C = "C", D = "D", E = "E", F = "F", G = "G", H = "H", I = "I", J = "J"
}

// CHECK-LABEL: sil [ossa] @$s4main14check_alphabetySiAA8AlphabetOF : $@convention(thin) (Alphabet) -> Int {
public func check_alphabet(_ state : Alphabet) -> Int {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == .E ? 1 : 0
}

// CHECK-LABEL: sil [ossa] @$s4main9compareItySbAA8AlphabetO_ADtF : $@convention(thin) (Alphabet, Alphabet) -> Bool {
public func compareIt(_ state : Alphabet, _ rhs: Alphabet) -> Bool {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == rhs
}

// int-enum.swift
public enum AlphabetInt : Int {
  case A = 10, B = 100, C = 12, D = 456, E = 1, F = 3, G = 77, H = 2, I = 27, J = 42
}

// CHECK-LABEL: sil [ossa] @$s4main18check_alphabet_intySiAA11AlphabetIntOF : $@convention(thin) (AlphabetInt) -> Int {
public func check_alphabet_int(_ state : AlphabetInt) -> Int {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == .E ? 1 : 0
}

// CHECK-LABEL: sil [ossa] @$s4main9compareItySbAA11AlphabetIntO_ADtF : $@convention(thin) (AlphabetInt, AlphabetInt) -> Bool {
public func compareIt(_ state : AlphabetInt, _ rhs: AlphabetInt) -> Bool {
  // FRAGILE: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  // RESILIENT: function_ref @$ss2eeoiySbx_xtSYRzSQ8RawValueRpzlF
  return state == rhs
}
