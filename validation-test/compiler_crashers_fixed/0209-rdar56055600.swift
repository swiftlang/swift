// RUN: %target-swift-frontend %s -emit-silgen -o /dev/null
let _: ([Int]) -> Int = \[Int].count
