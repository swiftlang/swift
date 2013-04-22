// RUN: %swift -emit-sil %s | FileCheck %s

operator infix =~ {}

func =~ (a:Int, b:Int) -> Bool { return a == b }

func the_real_life() -> Int { return 0 }
func just_fantasy() -> Int { return 1 }
func caught_in_a_landslide() -> Int { return 2 }

func foo() {}
func bar() {}
func bas() {}
func zim() {}
func zang() {}

// CHECK: sil @test1
func test1(x:Int) {
  switch x {
  case the_real_life():
    foo()
  default:
    bar()
  case just_fantasy():
    bas()
  }
  zim()
  // CHECK: constant_ref {{.*}} @the_real_life
  // CHECK: condbranch
  // CHECK: constant_ref {{.*}} @foo
  // CHECK: br [[CONT:bb[0-9]*]]
  // CHECK: constant_ref {{.*}} @just_fantasy
  // CHECK: condbranch
  // CHECK: constant_ref {{.*}} @bas
  // CHECK: br [[CONT]]
  // -- The default block is always emitted last, after all cases are checked
  // CHECK: constant_ref {{.*}} @bar
  // CHECK: br [[CONT]]
  // CHECK: [[CONT]]:
  // CHECK: constant_ref {{.*}} @zim
}

// CHECK: sil @test2
func test2(x:Int) {
  switch x {
  case the_real_life():
    foo()
  case just_fantasy():
    bas()
  }
  zim()
  // CHECK: constant_ref {{.*}} @the_real_life
  // CHECK: condbranch
  // CHECK: constant_ref {{.*}} @foo
  // CHECK: br [[CONT:bb[0-9]*]]
  // CHECK: constant_ref {{.*}} @just_fantasy
  // CHECK: condbranch
  // CHECK: constant_ref {{.*}} @bas
  // CHECK: br [[CONT]]
  // CHECK: [[CONT]]:
  // CHECK: constant_ref {{.*}} @zim
}

// CHECK: sil @test3
func test3(x:Int) {
  switch x {
  case the_real_life():
    foo()
    fallthrough
  default:
    bar()
    fallthrough
  case just_fantasy():
    bas()
    fallthrough
  case caught_in_a_landslide():
    zim()
  }
  zang()
  // CHECK: constant_ref {{.*}} @the_real_life
  // CHECK: condbranch
  // -- fallthrough follows source order, even though default happens last
  // CHECK: constant_ref {{.*}} @foo
  // CHECK: br [[DEFAULT:bb[0-9]*]]
  // CHECK: constant_ref {{.*}} @just_fantasy
  // CHECK: condbranch
  // CHECK: [[JUST_FANTASY:bb[0-9]*]]:
  // CHECK: constant_ref {{.*}} @bas
  // CHECK: br [[CAUGHT_IN_A_LANDSLIDE:bb[0-9]*]]
  // CHECK: constant_ref {{.*}} @caught_in_a_landslide
  // CHECK: condbranch
  // CHECK: [[CAUGHT_IN_A_LANDSLIDE]]:
  // CHECK: constant_ref {{.*}} @zim
  // CHECK: br [[CONT:bb[0-9]*]]
  // CHECK: [[DEFAULT]]:
  // CHECK: constant_ref {{.*}} @bar
  // CHECK: br [[JUST_FANTASY]]
  // CHECK: [[CONT]]:
  // CHECK: constant_ref {{.*}} @zang
}
