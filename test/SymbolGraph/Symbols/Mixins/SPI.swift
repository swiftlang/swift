// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SPIP.swift -module-name SPIP -emit-module -emit-module-path %t/ -include-spi-symbols
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -include-spi-symbols -I %t
// RUN: %target-swift-symbolgraph-extract -module-name SPI -I %t -pretty-print -output-dir %t -include-spi-symbols -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPI
// RUN: %FileCheck %s --input-file %t/SPI@SPIP.symbols.json --check-prefix SPI_EXT_SPIP
// RUN: %FileCheck %s --input-file %t/SPI@Swift.symbols.json --check-prefix SPI_EXT_Swift
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPIDOC

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SPIP.swift -module-name SPIP -emit-module -emit-module-path %t/ -include-spi-symbols
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -include-spi-symbols -I %t
// RUN: %target-swift-symbolgraph-extract -module-name SPI -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix NOSPI
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/SPI@SPIP.symbols.json
// RUN: %{python} -c 'import os.path; import sys; sys.exit(1 if os.path.exists(sys.argv[1]) else 0)' %t/SPI@Swift.symbols.json

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SPIP.swift -module-name SPIP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -include-spi-symbols -v
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPI-COMPILE
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix SPIDOC

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/SPIP.swift -module-name SPIP -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name SPI -emit-module -emit-module-path %t/SPI.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/SPI.symbols.json --check-prefix NOSPI-COMPILE

/// This is some struct, there
@_spi(SPI) public struct SomeStruct {}

// SPI: "precise": "s:3SPI10SomeStructV"
// SPI: "spi": true

// NOSPI-NOT: "precise": "s:3SPI10SomeStructV"

// SPI-COMPILE: s:3SPI10SomeStructV
// NOSPI-COMPILE-NOT: s:3SPI10SomeStructV

// SPIDOC: This is some struct, there

#if canImport(SPIP)
@_spi(SPI) import SPIP

public extension P {
    func foo() { }
}

@_spi(SPI)
extension String: P {}
#endif

// SPI_EXT_SPIP-DAG: "precise": "s:4SPIP1PP3SPIE3fooyyF"
// SPI_EXT_SPIP-DAG: "spi": true
// SPI_EXT_SPIP-DAG: "precise": "s:e:s:4SPIP1PP3SPIE3fooyyF"
// SPI_EXT_SPIP-DAG: "spi": true

// SPI_EXT_Swift-DAG: "precise": "s:e:s:SSs:4SPIP1PP"
// SPI_EXT_Swift-DAG: "spi": true
// SPI_EXT_Swift-DAG: "precise": "s:4SPIP1PP3SPIE3fooyyF::SYNTHESIZED::s:SS"
// SPI_EXT_Swift-DAG: "spi": true
