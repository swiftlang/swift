// RUN: %target-swift-frontend %s -emit-silgen

// rdar://23899799
let sol1  = { $1 ? "Dr. " + $0 : $0 }
sol1("Seuss", true)
