// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000

enum Cell {
    case alive, dead
}

struct Coord: Equatable {
    let x: Int, y: Int
    static func == (lhs: Coord, rhs: Coord) -> Bool { fatalError() }
}

extension Array {
    subscript(coord: Coord) -> Element { fatalError() }
}

let cells: [Cell] = [.alive]
let coord = Coord(x: 2, y: 4)
var count = 0

for a in -1...1 {
    (-1...1).lazy  // expected-error {{reasonable time}}
        .map { Coord(x: coord.x + a, y: coord.y + $0) }
        .filter { $0 != coord }
        .map { cells[$0] }
        .filter { $0 == .alive }
        .forEach { count += 1 }
}
