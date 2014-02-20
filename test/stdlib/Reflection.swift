// RUN: %target-run-simple-swift | FileCheck %s

// A non-reflective type.
struct Matte { val s: String }
val matteMirror = reflect(Matte("\(123)"))

// CHECK-LABEL: Matte:
// CHECK-NEXT:    no ID
// CHECK-NEXT:    0 children
// CHECK-NEXT:    text: <something>
// CHECK-NEXT:    no IDE repr
println("Matte:")
println(matteMirror.getObjectIdentifier() ? "has ID" : "no ID")
println("\(matteMirror.getCount()) children")
println("text: \(matteMirror.getString())")
println(matteMirror.getIDERepresentation() ? "has IDE repr" : "no IDE repr")

// A type that provides its own mirror.
struct BrilliantMirror : Mirror {
  val value: Brilliant
  func getValue() -> Any {
    return value
  }

  func getType() -> Any.metatype {
    return typeof(getValue())
  }

  func getObjectIdentifier() -> ObjectIdentifier? {
    // FIXME
    return nil
  }

  func getCount() -> Int {
    return 2
  }

  func getChild(i: Int) -> (String, Mirror) {
    switch i {
    case 0:
      return ("first", reflect(value.first))
    case 1:
      return ("second", reflect(value.second))
    case _:
      fatal("child index out of bounds")
    }
  }

  func getString() -> String {
    return "Brilliant(\(value.first), \(value.second))"
  }

  func getIDERepresentation() -> IDERepresentable? {
    return nil
  }
}

class Brilliant : Reflectable {
  val first: Int
  val second: String

  init(fst: Int, snd: String) {
    self.first = fst
    self.second = snd
  }

  func getMirror() -> Mirror {
    return BrilliantMirror(self)
  }
}

// CHECK-LABEL: Brilliant:
// CHECK-NEXT:    no ID
// CHECK-NEXT:    2 children
// CHECK-NEXT:      0: first => <something>
// CHECK-NEXT:      1: second => <something>
// CHECK-NEXT:    text: Brilliant(123, four five six)
// CHECK-NEXT:    no IDE repr
val brilliantMirror = reflect(Brilliant(123, "four five six"))
println("Brilliant:")
println(brilliantMirror.getObjectIdentifier() ? "has ID" : "no ID")
println("\(brilliantMirror.getCount()) children")
for i in 0...brilliantMirror.getCount() {
  val (name, child) = brilliantMirror.getChild(i)
  println("  \(i): \(name) => \(child.getString())")
}
println("text: \(brilliantMirror.getString())")
println(brilliantMirror.getIDERepresentation() ? "has IDE repr" : "no IDE repr")
