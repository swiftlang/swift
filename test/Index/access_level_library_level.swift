// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule | %FileCheck %s --check-prefix NO_LIBRARY_LEVEL
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule -library-level api | %FileCheck %s --check-prefix API_LIBRARY_LEVEL
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule -library-level spi | %FileCheck %s --check-prefix SPI_LIBRARY_LEVEL
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule -library-level ipi | %FileCheck %s --check-prefix IPI_LIBRARY_LEVEL
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule -library-level other | %FileCheck %s --check-prefix OTHER_LIBRARY_LEVEL

public func test() {}
// NO_LIBRARY_LEVEL: [[@LINE-1]]:13 | function(public)/Swift | test() | {{.*}} | Def | rel: 0
// API_LIBRARY_LEVEL: [[@LINE-2]]:13 | function(public)/Swift | test() | {{.*}} | Def | rel: 0
// SPI_LIBRARY_LEVEL: [[@LINE-3]]:13 | function(SPI)/Swift | test() | {{.*}} | Def | rel: 0
// IPI_LIBRARY_LEVEL: [[@LINE-4]]:13 | function(SPI)/Swift | test() | {{.*}} | Def | rel: 0
// OTHER_LIBRARY_LEVEL: [[@LINE-5]]:13 | function(public)/Swift | test() | {{.*}} | Def | rel: 0
