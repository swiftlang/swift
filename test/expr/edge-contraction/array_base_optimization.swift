// RUN: %target-typecheck-verify-swift

var fa2: Array<Double> = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
print("six has the value [\(fa2[0]), \(fa2[1]), \(fa2[2]), \(fa2[3]), \(fa2[4]), \(fa2[5])]")

var threeDoubles = Array(repeating: 0.0, count: 3)
var anotherThreeDoubles = Array(repeating: 2.5, count: 3)
var sixDoubles: [Double] = threeDoubles + anotherThreeDoubles
print("six has the value [\(sixDoubles[0]), \(sixDoubles[1]), \(sixDoubles[2]), \(sixDoubles[3]), \(sixDoubles[4]), \(sixDoubles[5])]")
