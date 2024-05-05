// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

/// Build the libraries.
// RUN: %target-swift-frontend -emit-module %t/ConformanceBaseTypes.swift -o %t -package-name pkg
// RUN: %target-swift-frontend -emit-module %t/ConformanceDefinition1.swift -o %t -I %t -package-name pkg
// RUN: %target-swift-frontend -emit-module %t/ConformanceDefinition2.swift -o %t -I %t -package-name pkg

/// Check diagnostics.
// RUN: %target-swift-frontend -typecheck -verify %t/ClientA.swift -I %t -package-name pkg
// RUN: %target-swift-frontend -typecheck -verify %t/ClientB.swift -I %t -package-name pkg

//--- ConformanceBaseTypes.swift
public protocol Proto {}
public struct ConformingType {
  public init () {}
}

package protocol PkgProto {} // expected-note 2 {{protocol 'PkgProto' is not '@usableFromInline' or public}}
package struct PkgConformingType { // expected-note 4 {{struct 'PkgConformingType' is not '@usableFromInline' or public}}
  package init () {} // expected-note 4 {{initializer 'init()' is not '@usableFromInline' or public}}
}

//--- ConformanceDefinition1.swift
import ConformanceBaseTypes
extension ConformingType : Proto  {}

//--- ConformanceDefinition2.swift
import ConformanceBaseTypes
extension PkgConformingType : PkgProto  {}

//--- ClientA.swift
public import ConformanceBaseTypes
internal import ConformanceDefinition1 // expected-note 2 {{extension of struct 'ConformingType' imported as 'internal' from 'ConformanceDefinition1' here}}
internal import ConformanceDefinition2 // expected-note 3 {{extension of struct 'PkgConformingType' imported as 'internal' from 'ConformanceDefinition2' here}}

public func useInAPI(a: any Proto = ConformingType()) { // expected-error {{cannot use conformance of 'ConformingType' to 'Proto' here; 'ConformanceDefinition1' was not imported publicly}}
}
public func useInAPI(b: any PkgProto = PkgConformingType()) {
  // expected-error@-1 {{cannot use conformance of 'PkgConformingType' to 'PkgProto' here; 'ConformanceDefinition2' was not imported publicly}}
  // expected-error@-2 {{function cannot be declared public because its parameter uses a package type}}
  // expected-error@-3 {{struct 'PkgConformingType' is package and cannot be referenced from a default argument value}}
  // expected-error@-4 {{initializer 'init()' is package and cannot be referenced from a default argument value}}
}
package func useInPkgAPI(a: any PkgProto = PkgConformingType()) { 
  // expected-error@-1 {{cannot use conformance of 'PkgConformingType' to 'PkgProto' here; 'ConformanceDefinition2' was not imported publicly}}
}

@inlinable public func inlinableFunc() {
  let _: any Proto = ConformingType() // expected-error {{cannot use conformance of 'ConformingType' to 'Proto' here; 'ConformanceDefinition1' was not imported publicly}}

  let _: any PkgProto = PkgConformingType() 
  // expected-error@-1 {{cannot use conformance of 'PkgConformingType' to 'PkgProto' here; 'ConformanceDefinition2' was not imported publicly}}
  // expected-error@-2 {{protocol 'PkgProto' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-3 {{struct 'PkgConformingType' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-4 {{initializer 'init()' is package and cannot be referenced from an '@inlinable' function}}
}

//--- ClientB.swift
public import ConformanceBaseTypes
package import ConformanceDefinition1 // expected-note 2 {{extension of struct 'ConformingType' imported as 'package' from 'ConformanceDefinition1' here}}
package import ConformanceDefinition2 // expected-note 2 {{extension of struct 'PkgConformingType' imported as 'package' from 'ConformanceDefinition2' here}}

public func useInAPI(a: any Proto = ConformingType()) { // expected-error {{cannot use conformance of 'ConformingType' to 'Proto' here; 'ConformanceDefinition1' was imported as package}}
}
public func useInAPI(b: any PkgProto = PkgConformingType()) { 
  // expected-error@-1 {{cannot use conformance of 'PkgConformingType' to 'PkgProto' here; 'ConformanceDefinition2' was imported as package}}
  // expected-error@-2 {{function cannot be declared public because its parameter uses a package type}}
  // expected-error@-3 {{struct 'PkgConformingType' is package and cannot be referenced from a default argument value}}
  // expected-error@-4 {{initializer 'init()' is package and cannot be referenced from a default argument value}}
}
package func useInPkgAPI(a: any PkgProto = PkgConformingType()) { // no-error
}

@inlinable public func inlinableFunc() {
  let _: any Proto = ConformingType() // expected-error {{cannot use conformance of 'ConformingType' to 'Proto' here; 'ConformanceDefinition1' was imported as package}}

  let _: any PkgProto = PkgConformingType()
  // expected-error@-1 {{cannot use conformance of 'PkgConformingType' to 'PkgProto' here; 'ConformanceDefinition2' was imported as package}}
  // expected-error@-2 {{protocol 'PkgProto' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-3 {{struct 'PkgConformingType' is package and cannot be referenced from an '@inlinable' function}}
  // expected-error@-4 {{initializer 'init()' is package and cannot be referenced from an '@inlinable' function}}
}
