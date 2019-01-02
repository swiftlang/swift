// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/onone %t/wmo
// RUN: %target-build-swift -emit-module -emit-module-path %t/onone/library.swiftmodule -module-name=library -emit-library %S/Inputs/library.swift -o %t/onone/%target-library-name(rary)
// RUN: %target-build-swift %S/main.swift -I %t/onone/ -o %t/onone/main -L%t/onone -lrary

// RUN: %target-build-swift -emit-module -emit-module-path %t/wmo/library.swiftmodule -module-name=library -emit-library -O -wmo %S/Inputs/library.swift -o %t/wmo/%target-library-name(rary)
// RUN: %target-build-swift %S/main.swift -I %t/wmo/ -o %t/wmo/main -L%t/wmo -lrary

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

public func launchToday(fc: FinalCountdown) {
  // Check if the setter is not transparent and therefore does not try to
  // reference the hidden offet variable symbol in the module.
  fc.count = 27
}

