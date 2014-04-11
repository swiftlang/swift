// RUN: %swift -parse %s -verify

let x: Bool = 3/4 as Float > 1/2 as Float
