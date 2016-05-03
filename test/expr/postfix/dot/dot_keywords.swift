// RUN: %target-parse-verify-swift

let x: Int = 1
let y: Int = x.self
let int: Int.Type = Int.self
