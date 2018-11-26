// RUN: %target-swift-frontend -emit-sil -O -primary-file %s -import-objc-header %S/Inputs/switch_enum_objc.h | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Osize -primary-file %s -import-objc-header %S/Inputs/switch_enum_objc.h | %FileCheck %s

@inline(never)
func action0() {}

@inline(never)
func action1(_: Int) {}

@inline(never)
func action2(_: Int, _: Int) {}

@inline(never)
func action3(_: Int, _: Int, _: Int) {}

@inline(never)
func action4(_: Int, _: Int, _: Int, _: Int) {}


// CHECK-LABEL: sil hidden @$S16switch_enum_objc14testImperativeyySo5AlphaVF
func testImperative(_ letter: Alpha) {
  // CHECK: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5, default bb6
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
  // CHECK: bb6:
  // CHECK: function_ref @$Ss32_diagnoseUnexpectedEnumCaseValue
} // CHECK: end sil function '$S16switch_enum_objc14testImperativeyySo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc27testImperativeDefaultMiddleyySo5AlphaVF
func testImperativeDefaultMiddle(_ letter: Alpha) {
  // CHECK: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, case #Alpha.e!enumelt: bb4, default bb5
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
    // CHECK: bb5:
    // CHECK: function_ref @$S16switch_enum_objc7action4
    action4(0, 0, 0, 0)
  }
} // CHECK: end sil function '$S16switch_enum_objc27testImperativeDefaultMiddleyySo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc24testImperativeDefaultEndyySo5AlphaVF
func testImperativeDefaultEnd(_ letter: Alpha) {
  // CHECK: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, default bb5
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
    // CHECK: bb5:
    // CHECK: function_ref @$S16switch_enum_objc7action4
    action4(0, 0, 0, 0)
  }
} // CHECK: end sil function '$S16switch_enum_objc24testImperativeDefaultEndyySo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc26testImperativeDefaultMultiyySo5AlphaVF
func testImperativeDefaultMulti(_ letter: Alpha) {
  // CHECK: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.d!enumelt: bb3, default bb4
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
    // CHECK: bb4:
    // CHECK: function_ref @$S16switch_enum_objc7action3
    action3(0, 0, 0)
  }
} // CHECK: end sil function '$S16switch_enum_objc26testImperativeDefaultMultiyySo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc14testFunctionalySiSo5AlphaVF
func testFunctional(_ letter: Alpha) -> Int {
  // This one can't be converted to select_enum because of the generated trap.
  // CHECK: switch_enum %0 : $Alpha, case #Alpha.a!enumelt: bb1, case #Alpha.b!enumelt: bb2, case #Alpha.c!enumelt: bb3, case #Alpha.d!enumelt: bb4, case #Alpha.e!enumelt: bb5, default bb6
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
  // CHECK: bb6:
  // CHECK: function_ref @$Ss32_diagnoseUnexpectedEnumCaseValue
} // CHECK: end sil function '$S16switch_enum_objc14testFunctionalySiSo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc27testFunctionalDefaultMiddleySiSo5AlphaVF
func testFunctionalDefaultMiddle(_ letter: Alpha) -> Int {
  // CHECK: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], case #Alpha.e!enumelt: [[THIRTEEN]], default [[TWENTY_ONE]] :
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
} // CHECK: end sil function '$S16switch_enum_objc27testFunctionalDefaultMiddleySiSo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc24testFunctionalDefaultEndySiSo5AlphaVF
func testFunctionalDefaultEnd(_ letter: Alpha) -> Int {
  // CHECK: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK: [[TWENTY_ONE:%.+]] = integer_literal ${{.+}}, 21
  // CHECK: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.c!enumelt: [[EIGHT]], case #Alpha.d!enumelt: [[THIRTEEN]], default [[TWENTY_ONE]] :
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
} // CHECK: end sil function '$S16switch_enum_objc24testFunctionalDefaultEndySiSo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc26testFunctionalDefaultMultiySiSo5AlphaVF
func testFunctionalDefaultMulti(_ letter: Alpha) -> Int {
  // CHECK: [[THREE:%.+]]      = integer_literal ${{.+}}, 3
  // CHECK: [[FIVE:%.+]]       = integer_literal ${{.+}}, 5
  // CHECK: [[EIGHT:%.+]]      = integer_literal ${{.+}}, 8
  // CHECK: [[THIRTEEN:%.+]]   = integer_literal ${{.+}}, 13
  // CHECK: = select_enum %0 : $Alpha, case #Alpha.a!enumelt: [[THREE]], case #Alpha.b!enumelt: [[FIVE]], case #Alpha.d!enumelt: [[EIGHT]], default [[THIRTEEN]] :
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
} // CHECK: end sil function '$S16switch_enum_objc26testFunctionalDefaultMultiySiSo5AlphaVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc19testImperativeHeadsyySo4CoinVF
func testImperativeHeads(_ coin: Coin) {
  // CHECK: switch_enum %0 : $Coin, case #Coin.heads!enumelt: bb2, default bb1
  // CHECK: bb1:
  // CHECK: function_ref @$S16switch_enum_objc7action1
  // CHECK: bb2:
  // CHECK: function_ref @$S16switch_enum_objc7action0
  if case .heads = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK: end sil function '$S16switch_enum_objc19testImperativeHeadsyySo4CoinVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc19testImperativeTailsyySo4CoinVF
func testImperativeTails(_ coin: Coin) {
  // CHECK: switch_enum %0 : $Coin, case #Coin.tails!enumelt: bb2, default bb1
  // CHECK: bb1:
  // CHECK: function_ref @$S16switch_enum_objc7action1
  // CHECK: bb2:
  // CHECK: function_ref @$S16switch_enum_objc7action0
  if case .tails = coin {
    action0()
  } else {
    action1(0)
  }
} // CHECK: end sil function '$S16switch_enum_objc19testImperativeTailsyySo4CoinVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc19testFunctionalHeadsySiSo4CoinVF
func testFunctionalHeads(_ coin: Coin) -> Int {
  // CHECK: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK: = select_enum %0 : $Coin, case #Coin.heads!enumelt: [[FIVE]], default [[NINE]]
  if case .heads = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK: end sil function '$S16switch_enum_objc19testFunctionalHeadsySiSo4CoinVF'

// CHECK-LABEL: sil hidden @$S16switch_enum_objc19testFunctionalTailsySiSo4CoinVF
func testFunctionalTails(_ coin: Coin) -> Int {
  // CHECK: [[FIVE:%.+]] = integer_literal ${{.+}}, 5000
  // CHECK: [[NINE:%.+]] = integer_literal ${{.+}}, 9001
  // CHECK: = select_enum %0 : $Coin, case #Coin.tails!enumelt: [[FIVE]], default [[NINE]]
  if case .tails = coin {
    return 5000
  } else {
    return 9001
  }
} // CHECK: end sil function '$S16switch_enum_objc19testFunctionalTailsySiSo4CoinVF'
