// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://github.com/swiftlang/swift/issues/48509

struct Calculator {

    let tCrit: Double
    let predictedValue: ([Double]) -> Double
}

func slow() {
  // variable indices
  let index1 = 0
  let index2 = 1
  let index3 = 2

  let _ = Calculator(
    tCrit: 1.962,
    predictedValue: { (parameters: [Double]) -> Double in
        return -603 + 123.31 * parameters[index1] + 9.288 * parameters[index2] + 0.316 * parameters[index3]
    }
  )

  let _ = Calculator(
    tCrit: 1.962,
    predictedValue: { parameters in
        -302.908 + 0.162 * parameters[index1] + 3.112 * parameters[index2] + 0.392 * parameters[index3]
  })

  let _ = Calculator(
    tCrit: 1.962,
    predictedValue: { parameters in
        -992 + 1.312 * parameters[index1] + 1.235 * parameters[index2]
  })

  let _ = Calculator(
    tCrit: 1.962,
    predictedValue: { parameters in
        -2932.32 + 1.253 * parameters[index1] + 3.212 * parameters[index2]
  })
}
