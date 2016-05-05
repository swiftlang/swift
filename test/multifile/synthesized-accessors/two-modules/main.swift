// RUN: rm -rf %t && mkdir %t

// RUN: mkdir %t/linker
// RUN: %target-build-swift -emit-module -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift -emit-library -c %S/library.swift -o %t/linker/library.o
// RUN: %target-build-swift %S/main.swift %t/linker/library.o -I %t/linker/ -L %t/linker/ -o %t/linker/main

// REQUIRES: executable_test

import library

protocol Takeaway {
  var costPounds: Float { get set }
  var costEuros: Float { get set }
  var costDollars: Float { get set }
}

extension FishAndChips: Takeaway {}

protocol Beverage {
  var abv: Int { get set }
}

extension Beer : Beverage {}

protocol PurrExtractor {
  var purrs: Int { get set }
}

extension LazyCat : PurrExtractor {}

// Dummy statement
_ = ()
