// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift \
// RUN:   -module-name Lib -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Lib.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/ClientA.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify
// RUN: %target-swift-frontend -typecheck %t/ClientB.swift -I %t -swift-version 6 -package-name mypkg -enable-library-evolution -verify

//--- ClientA.swift

@_implementationOnly import Lib
// expected-note@-1 {{protocol 'PkgProto' imported as 'internal' from 'Lib' here}}
// expected-note@-2 {{protocol 'PubProto' imported as 'internal' from 'Lib' here}}

public func f() -> PubProto? { 
  // expected-error@-1 {{cannot use protocol 'PubProto' here; 'Lib' has been imported as implementation-only}}
  // expected-error@-2 {{function cannot be declared public because its result uses an internal type}}
  return nil
}
package func g() -> PkgProto? { 
  // expected-error@-1 {{cannot use protocol 'PkgProto' here; 'Lib' has been imported as implementation-only}}
  // expected-error@-2 {{function cannot be declared package because its result uses an internal type}}
  return nil
}


//--- ClientB.swift

package import Lib // no-warning

package struct MyStruct: PubProto {
  package var someVar = ""
}


//--- Lib.swift

// expected-note@+1 2{{type declared here}}
public protocol PubProto {
  var someVar: String { get set }
}

// expected-note@+1 2{{type declared here}}
package protocol PkgProto {
  var someVar: String { get set }
}

