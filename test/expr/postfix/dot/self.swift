// RUN: %swift -parse -verify %s

val x: Int = 1
val y: Int = x.self
val int: Int.Type = Int.self
