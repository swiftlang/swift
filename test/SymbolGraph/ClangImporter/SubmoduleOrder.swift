// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-symbolgraph-extract -sdk %clang-importer-sdk -module-name Many -I %t -output-dir %t -pretty-print -v
// RUN: %FileCheck %s --input-file %t/Many.symbols.json

//--- module.modulemap
module Many {
  export *
  module Hotel { header "Hotel.h" }
  module Echo { header "Echo.h" }
  module Golf { header "Golf.h" }
  module Alpha { header "Alpha.h" }
  module Foxtrot { header "Foxtrot.h" }
  module Charlie { header "Charlie.h" }
  module Bravo { header "Bravo.h" }
  module Delta { header "Delta.h" }
}

//--- Hotel.h
void hotel(void);

//--- Echo.h
void echo(void);

//--- Golf.h
void golf(void);

//--- Alpha.h
void alpha(void);

//--- Foxtrot.h
void foxtrot(void);

//--- Charlie.h
void charlie(void);

//--- Bravo.h
void bravo(void);

//--- Delta.h
void delta(void);

// Symbols from re-exported submodules must be emitted in declaration order.
// CHECK: "precise": "c:@F@hotel"
// CHECK: "precise": "c:@F@echo"
// CHECK: "precise": "c:@F@golf"
// CHECK: "precise": "c:@F@alpha"
// CHECK: "precise": "c:@F@foxtrot"
// CHECK: "precise": "c:@F@charlie"
// CHECK: "precise": "c:@F@bravo"
// CHECK: "precise": "c:@F@delta"
