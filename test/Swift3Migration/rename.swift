// RUN: %swift -parse -target %target-triple %s -fixit-all -emit-fixits-path %t.remap -swift3-migration
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

@swift3_migration(renamed="A")
struct X<@swift3_migration(renamed="Element") T> {
  @swift3_migration(renamed="init(b:)")
  init(withY: Y) { }

  @swift3_migration(renamed="bar(wibble:wonka:)")
  func foo(x: Int, y: Int) { }

  init(other y: Y) {
    self.init(withY: y)
  }
}

@swift3_migration(renamed="B")
struct Y { };


X<Y>(withY: Y()).foo(1, y: 2)

