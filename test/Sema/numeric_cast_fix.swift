// RUN: %target-swift-frontend -typecheck -verify -primary-file %s


let x: [Int] = [1, 2, 3, 4]
let y: UInt = 4

_ = x.filter { ($0 + y)  > 42 } // expected-error {{cannot convert value of type 'UInt' to expected argument type 'Int'}} {{22-22=numericCast(}} {{23-23=)}}
