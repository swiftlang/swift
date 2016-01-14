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

  @swift3_migration(renamedToProperty="wobbleProp")
  func wobble() -> Int {
    return 5
  }
}

@swift3_migration(renamed="B")
struct Y { };


X<Y>(withY: Y()).foo(1, y: 2)

func test(xi: X<Int>) {
  _ = xi.wobble()
}

protocol P {
  @swift3_migration(renamed="Assoc")
  associatedtype AssocType

  @swift3_migration(renamed="generateAssoc()")
  func getAssoc() -> AssocType
}

struct ConformsToP : P {
  func getAssoc() -> Int { return 0 }
}

let someInt: ConformsToP.AssocType = 0
