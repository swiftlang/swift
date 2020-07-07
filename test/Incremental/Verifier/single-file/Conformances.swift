// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S
// RUN: cd %t && %target-swiftc_driver -typecheck -output-file-map %t/output.json -incremental -disable-direct-intramodule-dependencies -module-name main -verify-incremental-dependencies %s

public protocol PublicProtocol { } // expected-provides {{PublicProtocol}}
internal protocol InternalProtocol { } // expected-provides {{InternalProtocol}}
fileprivate protocol FilePrivateProtocol { } // expected-provides {{FilePrivateProtocol}}
private protocol PrivateProtocol { } // expected-provides {{PrivateProtocol}}

public struct PublicConformance { } // expected-provides {{PublicConformance}}
// expected-cascading-member {{main.PublicConformance.init}}

// expected-cascading-conformance {{main.PublicConformance}}
extension PublicConformance: PublicProtocol { }
extension PublicConformance: InternalProtocol { }
extension PublicConformance: FilePrivateProtocol { }
extension PublicConformance: PrivateProtocol { }


private struct PrivateConformance { } // expected-provides {{PrivateConformance}}
// expected-cascading-member {{main.PrivateConformance.init}}

// expected-cascading-conformance {{main.PrivateConformance}}
extension PrivateConformance: PublicProtocol { } // expected-cascading-conformance {{main.PublicProtocol}}
extension PrivateConformance: InternalProtocol { } // expected-cascading-conformance {{main.InternalProtocol}}
extension PrivateConformance: FilePrivateProtocol { } // expected-cascading-conformance {{main.FilePrivateProtocol}}
extension PrivateConformance: PrivateProtocol { } // expected-cascading-conformance {{main.PrivateProtocol}}

