// RUN: %target-swift-emit-silgen -parse-stdlib -module-name keypaths %s | %FileCheck %s

import Swift

enum Color {
  case red
  case green
  case blue
  case generic(String)
}

enum GenericColor<T> {
  case red
  case green
  case blue
  case generic(T)
}

func enumCases(_: Color) {
  if #available(SwiftStdlib 5.0, *) {
    // CHECK: keypath $KeyPath<Color, Optional<String>>, (root $Color; computed_enum_case #Color.generic : $Optional<String>, getter @$s8keypaths5ColorO7genericyACSScACmFACTK : $@convention(thin) (@in_guaranteed Color) -> @out Optional<String>)
    let _ = \Color.generic

    // CHECK: keypath $KeyPath<Optional<(Int, Int)>, Optional<Int>>, (root $Optional<(Int, Int)>; optional_chain : $(Int, Int); tuple_element #1 : $Int; optional_wrap : $Optional<Int>)
    let _ = \(Int, Int)?.some?.1
  }
}

func enumCasesGeneric<T, U>(_: T, _: U) {
  if #available(SwiftStdlib 5.0, *) {
    // CHECK: keypath $KeyPath<Optional<(T, U)>, Optional<U>>, <τ_0_0, τ_0_1> (root $Optional<(τ_0_0, τ_0_1)>; optional_chain : $(τ_0_0, τ_0_1); tuple_element #1 : $τ_0_1; optional_wrap : $Optional<τ_0_1>) <T, U>
    let _ = \(T, U)?.some?.1

    // CHECK: keypath $KeyPath<GenericColor<T>, Optional<T>>, <τ_0_0, τ_0_1> (root $GenericColor<τ_0_0>; computed_enum_case #GenericColor.generic : $Optional<τ_0_0>, getter @$s8keypaths12GenericColorO7genericyACyxGxcAEmlFr0_lAETK : $@convention(thin) <τ_0_0, τ_0_1> (@in_guaranteed GenericColor<τ_0_0>) -> @out Optional<τ_0_0>) <T, U>
    let _ = \GenericColor<T>.generic
  }
}