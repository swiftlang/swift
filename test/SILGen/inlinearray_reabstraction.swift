// RUN: %target-swift-emit-silgen -disable-availability-checking -verify %s


let a: InlineArray<_, (Int)->Int> = [{$0*2}]
print(a[0](3))
