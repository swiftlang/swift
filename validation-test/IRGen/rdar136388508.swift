// RUN: %target-swiftc_driver -c -O %s

// This used to crash during LargeTypesReg2Mem.

class C {}
struct Big {
  let c: C
  let x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20: Int
  @_silgen_name("Big.init")
  init(_ bool: Bool)
}

@_silgen_name("summon")
func summon<Entity>() throws -> Entity
@_silgen_name("banish")
func banish<Entity>(_ t: Entity)

func doit() {
  let big: Big
  do {
      big = Big(try summon())
      
      banish(try summon() as Bool)
  } catch {
      _ = "\(error)"
  }
}
