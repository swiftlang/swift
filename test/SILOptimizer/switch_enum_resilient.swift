// RUN: %empty-directory(%t)

// Check all combinations of -O/-Osize, fragile/resilient, and SIL within the
// module vs. inlinable SIL.

// RUN: %target-swift-frontend -emit-sil -O -primary-file %s -emit-module-path %t/O-fragile.swiftmodule | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-NOINLINE -check-prefix CHECK-FRAGILE -check-prefix CHECK-FRAGILE-NOINLINE %s
// RUN: %target-sil-opt %t/O-fragile.swiftmodule -module-name switch_enum_resilient -emit-sorted-sil -disable-sil-linking | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-FRAGILE %s

// RUN: %target-swift-frontend -emit-sil -Osize -primary-file %s -emit-module-path %t/Osize-fragile.swiftmodule | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-NOINLINE -check-prefix CHECK-FRAGILE -check-prefix CHECK-FRAGILE-NOINLINE %s
// RUN: %target-sil-opt %t/Osize-fragile.swiftmodule -module-name switch_enum_resilient -emit-sorted-sil -disable-sil-linking | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-FRAGILE %s

// RUN: %target-swift-frontend -emit-sil -enable-resilience -O -primary-file %s -emit-module-path %t/O-resilient.swiftmodule | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-NOINLINE -check-prefix=CHECK-RESILIENT -check-prefix=CHECK-RESILIENT-NOINLINE %s
// RUN: %target-sil-opt %t/O-resilient.swiftmodule -module-name switch_enum_resilient -emit-sorted-sil -disable-sil-linking | %FileCheck -check-prefix CHECK-ALL -check-prefix=CHECK-RESILIENT -check-prefix CHECK-RESILIENT-INLINE %s

// RUN: %target-swift-frontend -emit-sil -enable-resilience -Osize -primary-file %s -emit-module-path %t/Osize-resilient.swiftmodule | %FileCheck -check-prefix CHECK-ALL -check-prefix CHECK-NOINLINE -check-prefix=CHECK-RESILIENT -check-prefix=CHECK-RESILIENT-NOINLINE %s
// RUN: %target-sil-opt %t/Osize-resilient.swiftmodule -module-name switch_enum_resilient -emit-sorted-sil -disable-sil-linking | %FileCheck -check-prefix CHECK-ALL -check-prefix=CHECK-RESILIENT -check-prefix CHECK-RESILIENT-INLINE %s

public enum Alpha : Int {
  case a, b, c, d, e
}

@inline(never)
public func action0() {}

@inline(never)
public func action1(_: Int) {}

@inline(never)
public func action2(_: Int, _: Int) {}

@inline(never)
public func action3(_: Int, _: Int, _: Int) {}

@inline(never)
public func action4(_: Int, _: Int, _: Int, _: Int) {}


// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient14testImperativeyyAA5AlphaOF
public func testImperative(_ letter: Alpha) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Alpha| %0 : [$]Alpha}}, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  case .c:
    action2(0, 0)
  case .d:
    action3(0, 0, 0)
  case .e:
    action4(0, 0, 0, 0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient14testImperativeyyAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient27testImperativeDefaultMiddleyyAA5AlphaOF
public func testImperativeDefaultMiddle(_ letter: Alpha) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Alpha| %0 : [$]Alpha}}, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, case #Alpha.e!enumelt: bb4, case #Alpha.c!enumelt: bb5 //
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  // case .c:
  case .d:
    action2(0, 0)
  case .e:
    action3(0, 0, 0)
  default:
    // CHECK-NOINLINE: bb5:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action4
    action4(0, 0, 0, 0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient27testImperativeDefaultMiddleyyAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient24testImperativeDefaultEndyyAA5AlphaOF
public func testImperativeDefaultEnd(_ letter: Alpha) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Alpha| %0 : [$]Alpha}}, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  case .c:
    action2(0, 0)
  case .d:
    action3(0, 0, 0)
  // case .e:
  default:
    // CHECK-NOINLINE: bb5:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action4
    action4(0, 0, 0, 0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient24testImperativeDefaultEndyyAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient26testImperativeDefaultMultiyyAA5AlphaOF
public func testImperativeDefaultMulti(_ letter: Alpha) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Alpha| %0 : [$]Alpha}}, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, default bb4
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  // case .c:
  case .d:
    action2(0, 0)
  // case .e:
  default:
    // CHECK-NOINLINE: bb4:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action3
    action3(0, 0, 0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient26testImperativeDefaultMultiyyAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient14testFunctionalySiAA5AlphaOF
public func testFunctional(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE-NOINLINE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE-NOINLINE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE-NOINLINE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.c!enumelt: [[EIGHT]], case #Alpha.d!enumelt: [[THIRTEEN]], case #Alpha.e!enumelt: [[TWENTY_ONE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  case .c:
    return 8
  case .d:
    return 13
  case .e:
    return 21
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient14testFunctionalySiAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient27testFunctionalDefaultMiddleySiAA5AlphaOF
public func testFunctionalDefaultMiddle(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE-NOINLINE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE-NOINLINE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE-NOINLINE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], case #Alpha.e!enumelt: [[THIRTEEN]], case #Alpha.c!enumelt: [[TWENTY_ONE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, case #Alpha.e!enumelt: bb4, case #Alpha.c!enumelt: bb5 //
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  // case .c:
  case .d:
    return 8
  case .e:
    return 13
  default:
    return 21
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient27testFunctionalDefaultMiddleySiAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient24testFunctionalDefaultEndySiAA5AlphaOF
public func testFunctionalDefaultEnd(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE-NOINLINE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE-NOINLINE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE-NOINLINE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.c!enumelt: [[EIGHT]], case #Alpha.d!enumelt: [[THIRTEEN]], case #Alpha.e!enumelt: [[TWENTY_ONE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  case .c:
    return 8
  case .d:
    return 13
  // case .e:
  default:
    return 21
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient24testFunctionalDefaultEndySiAA5AlphaOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient26testFunctionalDefaultMultiySiAA5AlphaOF
public func testFunctionalDefaultMulti(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE-NOINLINE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE-NOINLINE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], default [[THIRTEEN]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, default bb4
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  // case .c:
  case .d:
    return 8
  // case .e:
  default:
    return 13
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient26testFunctionalDefaultMultiySiAA5AlphaOF'

public enum Coin : Int {
  case heads, tails
}

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient19testImperativeHeadsyyAA4CoinOF
public func testImperativeHeads(_ coin: Coin) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Coin| %0 : [$]Coin}}, case #Coin.heads!enumelt: bb1, case #Coin.tails!enumelt: bb2 //
  if case .heads = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient19testImperativeHeadsyyAA4CoinOF'

// CHECK-NOINLINE-LABEL: sil{{.*}} @$s21switch_enum_resilient19testImperativeTailsyyAA4CoinOF
public func testImperativeTails(_ coin: Coin) {
  // CHECK-NOINLINE: switch_enum{{_addr %.+ : [$][*]Coin| %0 : [$]Coin}}, case #Coin.tails!enumelt: bb1, case #Coin.heads!enumelt: bb2 //
  if case .tails = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient19testImperativeTailsyyAA4CoinOF'

// CHECK-NOINLINE-LABEL: sil @$s21switch_enum_resilient19testFunctionalHeadsySiAA4CoinOF
public func testFunctionalHeads(_ coin: Coin) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK-FRAGILE-NOINLINE: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Coin, case #Coin.heads!enumelt: [[FIVE]], case #Coin.tails!enumelt: [[NINE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.heads!enumelt: bb1, case #Coin.tails!enumelt: bb2
  if case .heads = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient19testFunctionalHeadsySiAA4CoinOF'

// CHECK-NOINLINE-LABEL: sil @$s21switch_enum_resilient19testFunctionalTailsySiAA4CoinOF
public func testFunctionalTails(_ coin: Coin) -> Int {
  // CHECK-FRAGILE-NOINLINE: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK-FRAGILE-NOINLINE: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK-FRAGILE-NOINLINE: = select_enum %0 : $Coin, case #Coin.tails!enumelt: [[FIVE]], case #Coin.heads!enumelt: [[NINE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.tails!enumelt: bb1, case #Coin.heads!enumelt: bb2
  if case .tails = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK-NOINLINE: end sil function '$s21switch_enum_resilient19testFunctionalTailsySiAA4CoinOF'

// *** The following are in -emit-sorted-sil order ***

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient16inlineFunctionalySiAA5AlphaOF
@inlinable public func inlineFunctional(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.c!enumelt: [[EIGHT]], case #Alpha.d!enumelt: [[THIRTEEN]], case #Alpha.e!enumelt: [[TWENTY_ONE]] :

  // This one can't be converted to select_enum because of the generated trap.
  // CHECK-RESILIENT: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  case .c:
    return 8
  case .d:
    return 13
  case .e:
    return 21
  }
  // CHECK-RESILIENT-NOINLINE: bb6:
  // CHECK-RESILIENT-NOINLINE: function_ref @$ss27_diagnoseUnexpectedEnumCase
} // CHECK-ALL: end sil function '$s21switch_enum_resilient16inlineFunctionalySiAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient16inlineImperativeyyAA5AlphaOF
@inlinable public func inlineImperative(_ letter: Alpha) {
  // CHECK-FRAGILE: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}} //
  // CHECK-RESILIENT: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  case .c:
    action2(0, 0)
  case .d:
    action3(0, 0, 0)
  case .e:
    action4(0, 0, 0, 0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient16inlineImperativeyyAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient21inlineFunctionalHeadsySiAA4CoinOF
@inlinable public func inlineFunctionalHeads(_ coin: Coin) -> Int {
  // CHECK-FRAGILE: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK-FRAGILE: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK-FRAGILE: = select_enum %0 : $Coin, case #Coin.heads!enumelt: [[FIVE]], case #Coin.tails!enumelt: [[NINE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.heads!enumelt: bb{{[0-9]+}}, case #Coin.tails!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.heads!enumelt: bb2, default bb1
  if case .heads = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient21inlineFunctionalHeadsySiAA4CoinOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient21inlineFunctionalTailsySiAA4CoinOF
@inlinable public func inlineFunctionalTails(_ coin: Coin) -> Int {
  // CHECK-FRAGILE: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK-FRAGILE: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK-FRAGILE: = select_enum %0 : $Coin, case #Coin.tails!enumelt: [[FIVE]], case #Coin.heads!enumelt: [[NINE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.tails!enumelt: bb{{[0-9]+}}, case #Coin.heads!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.tails!enumelt: bb2, default bb1
  if case .tails = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient21inlineFunctionalTailsySiAA4CoinOF'


// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient21inlineImperativeHeadsyyAA4CoinOF
@inlinable public func inlineImperativeHeads(_ coin: Coin) {
  // CHECK-FRAGILE: switch_enum %0 : $Coin, case #Coin.heads!enumelt: bb{{[0-9]+}}, case #Coin.tails!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.heads!enumelt: bb{{[0-9]+}}, case #Coin.tails!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.heads!enumelt: bb2, default bb1
  if case .heads = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient21inlineImperativeHeadsyyAA4CoinOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient21inlineImperativeTailsyyAA4CoinOF
@inlinable public func inlineImperativeTails(_ coin: Coin) {
  // CHECK-FRAGILE: switch_enum %0 : $Coin, case #Coin.tails!enumelt: bb{{[0-9]+}}, case #Coin.heads!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.tails!enumelt: bb{{[0-9]+}}, case #Coin.heads!enumelt: bb{{[0-9]+}} //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Coin, case #Coin.tails!enumelt: bb2, default bb1
  if case .tails = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient21inlineImperativeTailsyyAA4CoinOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient26inlineFunctionalDefaultEndySiAA5AlphaOF
@inlinable public func inlineFunctionalDefaultEnd(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.c!enumelt: [[EIGHT]], case #Alpha.d!enumelt: [[THIRTEEN]], case #Alpha.e!enumelt: [[TWENTY_ONE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  case .c:
    return 8
  case .d:
    return 13
  // case .e:
  default:
    return 21
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient26inlineFunctionalDefaultEndySiAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient26inlineImperativeDefaultEndyyAA5AlphaOF
@inlinable public func inlineImperativeDefaultEnd(_ letter: Alpha) {
  // CHECK-FRAGILE: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}} //
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5 //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  case .c:
    action2(0, 0)
  case .d:
    action3(0, 0, 0)
  // case .e:
  default:
    // CHECK-NOINLINE: bb5:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action4
    action4(0, 0, 0, 0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient26inlineImperativeDefaultEndyyAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient28inlineFunctionalDefaultMultiySiAA5AlphaOF
@inlinable public func inlineFunctionalDefaultMulti(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], default [[THIRTEEN]] :
  // CHECK-RESILIENT: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  // case .c:
  case .d:
    return 8
  // case .e:
  default:
    return 13
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient28inlineFunctionalDefaultMultiySiAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient28inlineImperativeDefaultMultiyyAA5AlphaOF
@inlinable public func inlineImperativeDefaultMulti(_ letter: Alpha) {
  // CHECK-ALL: switch_enum{{_addr %.+ : [$][*]Alpha| %0 : [$]Alpha}}, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  // case .c:
  case .d:
    action2(0, 0)
  // case .e:
  default:
    // CHECK-NOINLINE: bb4:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action3
    action3(0, 0, 0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient28inlineImperativeDefaultMultiyyAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient29inlineFunctionalDefaultMiddleySiAA5AlphaOF
@inlinable public func inlineFunctionalDefaultMiddle(_ letter: Alpha) -> Int {
  // CHECK-FRAGILE: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK-FRAGILE: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK-FRAGILE: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK-FRAGILE: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK-FRAGILE: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK-FRAGILE: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], case #Alpha.e!enumelt: [[THIRTEEN]], case #Alpha.c!enumelt: [[TWENTY_ONE]] :
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, case #Alpha.e!enumelt: bb4, case #Alpha.c!enumelt: bb5 //
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    return 3
  case .b:
    return 5
  // case .c:
  case .d:
    return 8
  case .e:
    return 13
  default:
    return 21
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient29inlineFunctionalDefaultMiddleySiAA5AlphaOF'

// CHECK-ALL-LABEL: sil{{.*}} @$s21switch_enum_resilient29inlineImperativeDefaultMiddleyyAA5AlphaOF
@inlinable public func inlineImperativeDefaultMiddle(_ letter: Alpha) {
  // CHECK-FRAGILE: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}}, case #Alpha.c!enumelt: {{bb.+}} //
  // CHECK-RESILIENT-NOINLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, case #Alpha.e!enumelt: bb4, case #Alpha.c!enumelt: bb5
  // CHECK-RESILIENT-INLINE: switch_enum_addr {{%.+}} : $*Alpha, case #Alpha.a!enumelt: {{bb.+}}, case #Alpha.b!enumelt: {{bb.+}}, case #Alpha.d!enumelt: {{bb.+}}, case #Alpha.e!enumelt: {{bb.+}}, default {{bb.+}}
  switch letter {
  case .a:
    action0()
  case .b:
    action1(0)
  // case .c:
  case .d:
    action2(0, 0)
  case .e:
    action3(0, 0, 0)
  default:
    // CHECK-NOINLINE: bb5:
    // CHECK-NOINLINE: function_ref @$s21switch_enum_resilient7action4
    action4(0, 0, 0, 0)
  }
} // CHECK-ALL: end sil function '$s21switch_enum_resilient29inlineImperativeDefaultMiddleyyAA5AlphaOF'
