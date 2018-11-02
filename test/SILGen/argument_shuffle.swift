// RUN: %target-swift-emit-silgen -enable-sil-ownership %s

struct Horse<T> {
  func walk(_: (String, Int), reverse: Bool) {}
  func trot(_: (x: String, y: Int), halfhalt: Bool) {}
  func canter(_: T, counter: Bool) {}
}

var kevin = Horse<(x: String, y: Int)>()
var loki = Horse<(String, Int)>()

//

// No conversion
let noLabelsTuple = ("x", 1)
kevin.walk(("x", 1), reverse: false)
kevin.walk(noLabelsTuple, reverse: false)

loki.canter(("x", 1), counter: false)
loki.canter(noLabelsTuple, counter: false)

// Introducing labels
kevin.trot(("x", 1), halfhalt: false)
kevin.trot(noLabelsTuple, halfhalt: false)

kevin.canter(("x", 1), counter: false)
kevin.canter(noLabelsTuple, counter: false)

// Eliminating labels
let labelsTuple = (x: "x", y: 1)
kevin.walk((x: "x", y: 1), reverse: false)
kevin.walk(labelsTuple, reverse: false)

loki.canter((x: "x", y: 1), counter: false)
loki.canter(labelsTuple, counter: false)

// No conversion
kevin.trot((x: "x", y: 1), halfhalt: false)
kevin.trot(labelsTuple, halfhalt: false)

kevin.canter((x: "x", y: 1), counter: false)
kevin.canter(labelsTuple, counter: false)

// Shuffling labels
let shuffledLabelsTuple = (y: 1, x: "x")
kevin.trot((y: 1, x: "x"), halfhalt: false)
kevin.trot(shuffledLabelsTuple, halfhalt: false)

kevin.canter((y: 1, x: "x"), counter: false)
kevin.canter(shuffledLabelsTuple, counter: false)
