// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

indirect enum TreeA<T> {
  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeA3NilurFMGS0_q__GS0_q__ : $@convention(thin) <T> (@thin TreeA<T>.Type) -> @owned TreeA<T> {
  // CHECK:         enum $TreeA<T>, #TreeA.Nil!enumelt
  case Nil


  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeA4LeafurfMGS0_q__Fq_GS0_q__ : $@convention(thin) <T> (@in T, @thin TreeA<T>.Type) -> @owned TreeA<T> {
  // CHECK:         [[BOX:%.*]] = alloc_box $T
  // CHECK:         copy_addr [take] %0 to [initialization] [[BOX]]#1 : $*T
  // CHECK:         enum $TreeA<T>, #TreeA.Leaf!enumelt.1, [[BOX]]#0 : $@box T
  case Leaf(T)

  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeA6BranchurfMGS0_q__FT4leftGS0_q__5rightGS0_q___GS0_q__
  // CHECK:         [[TUPLE:%.*]] = tuple $(left: TreeA<T>, right: TreeA<T>) (%0, %1)
  // CHECK:         [[BOX:%.*]] = alloc_box $(left: TreeA<T>, right: TreeA<T>)
  // CHECK:         store [[TUPLE]] to [[BOX]]#1 : $*(left: TreeA<T>, right: TreeA<T>)
  // CHECK:         enum $TreeA<T>, #TreeA.Branch!enumelt.1, [[BOX]]#0 : $@box (left: TreeA<T>, right: TreeA<T>)
  case Branch(left: TreeA<T>, right: TreeA<T>)
}

enum TreeB<T> {
  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeB3NilurFMGS0_q__GS0_q__ : $@convention(thin) <T> (@out TreeB<T>, @thin TreeB<T>.Type) -> () {
  // CHECK:         inject_enum_addr %0 : $*TreeB<T>, #TreeB.Nil!enumelt
  case Nil

  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeB4LeafurfMGS0_q__Fq_GS0_q__ : $@convention(thin) <T> (@out TreeB<T>, @in T, @thin TreeB<T>.Type) -> () {
  // CHECK:         [[ADDR:%.*]] = init_enum_data_addr %0 : $*TreeB<T>, #TreeB.Leaf!enumelt.1
  // CHECK:         copy_addr [take] %1 to [initialization] [[ADDR]] : $*T
  // CHECK:         inject_enum_addr %0 : $*TreeB<T>, #TreeB.Leaf!enumelt.1
  case Leaf(T)

  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum5TreeB6BranchurfMGS0_q__FT4leftGS0_q__5rightGS0_q___GS0_q__ : $@convention(thin) <T> (@out TreeB<T>, @in TreeB<T>, @in TreeB<T>, @thin TreeB<T>.Type) -> ()
  // CHECK:         [[TUPLE:%.*]] = alloc_stack $(left: TreeB<T>, right: TreeB<T>
  // CHECK:         [[BOX:%.*]] = alloc_box $(left: TreeB<T>, right: TreeB<T>)
  // CHECK:         copy_addr [take] [[TUPLE]]#1 to [initialization] [[BOX]]#1 : $*(left: TreeB<T>, right: TreeB<T>)
  // CHECK:         [[ADDR:%.*]] = init_enum_data_addr %0 : $*TreeB<T>, #TreeB.Branch!enumelt.1
  // CHECK:         store [[BOX]]#0 to [[ADDR]] : $*@box (left: TreeB<T>, right: TreeB<T>)
  // CHECK:         inject_enum_addr %0 : $*TreeB<T>, #TreeB.Branch!enumelt.1
  indirect case Branch(left: TreeB<T>, right: TreeB<T>)
}

enum TreeInt {
  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum7TreeInt3NilFMS0_S0_ : $@convention(thin) (@thin TreeInt.Type) -> @owned TreeInt {
  // CHECK:         enum $TreeInt, #TreeInt.Nil!enumelt
  case Nil

  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum7TreeInt4LeaffMS0_FSiS0_ : $@convention(thin) (Int, @thin TreeInt.Type) -> @owned TreeInt
  // CHECK:         enum $TreeInt, #TreeInt.Leaf!enumelt.1, %0 : $Int
  case Leaf(Int)

  // CHECK-LABEL: sil hidden [transparent] @_TFO13indirect_enum7TreeInt6BranchfMS0_FT4leftS0_5rightS0__S0_ : $@convention(thin) (@owned TreeInt, @owned TreeInt, @thin TreeInt.Type) -> @owned TreeInt 
  // CHECK:         [[TUPLE:%.*]] = tuple $(left: TreeInt, right: TreeInt) (%0, %1)
  // CHECK:         [[BOX:%.*]] = alloc_box $(left: TreeInt, right: TreeInt)
  // CHECK:         store [[TUPLE]] to [[BOX]]#1 : $*(left: TreeInt, right: TreeInt)
  // CHECK:         enum $TreeInt, #TreeInt.Branch!enumelt.1, [[BOX]]#0 : $@box (left: TreeInt, right: TreeInt)
  indirect case Branch(left: TreeInt, right: TreeInt)
}

