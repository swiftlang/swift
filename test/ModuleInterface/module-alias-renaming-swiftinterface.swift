/// Check that we can alias a module imported from a swiftinterface
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name Lib \
// RUN:     -swift-version 5 -enable-library-evolution \
// RUN:     -o %t/Lib.swiftmodule \
// RUN:     -emit-module-interface-path %t/Lib.swiftinterface \
// RUN:     %t/Lib.swift

/// We can alias an imported module built from a swiftmodule
// RUN: %target-swift-frontend -typecheck -module-name Client \
// RUN:     -swift-version 5 \
// RUN:     -module-alias AliasedLib=Lib \
// RUN:     %t/Client.swift -I%t

/// We can alias an imported module built from a swiftinterface
// RUN: rm %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck -module-name Client \
// RUN:     -swift-version 5 \
// RUN:     -module-alias AliasedLib=Lib \
// RUN:     %t/Client.swift -I%t

//--- Lib.swift
public func foo() {}

//--- Client.swift
import AliasedLib

func main() {
  AliasedLib.foo()
}
