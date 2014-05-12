// RUN: %swift -parse -verify %s

let x: Int = 1
let y: Int = x.self
let int: Int.Type = Int.self
