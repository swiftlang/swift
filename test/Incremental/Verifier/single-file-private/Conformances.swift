// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S
// RUN: cd %t && %target-swiftc_driver -typecheck -output-file-map %t/output.json -incremental -module-name main -enable-direct-intramodule-dependencies -verify-incremental-dependencies %s

public protocol PublicProtocol { } // expected-provides {{PublicProtocol}}
internal protocol InternalProtocol { } // expected-provides {{InternalProtocol}}
fileprivate protocol FilePrivateProtocol { } // expected-provides {{FilePrivateProtocol}}
private protocol PrivateProtocol { } // expected-provides {{PrivateProtocol}}

public struct PublicConformance { } // expected-provides {{PublicConformance}}
// expected-private-member {{main.PublicConformance.init}}

// expected-private-conformance {{main.PublicConformance}}
extension PublicConformance: PublicProtocol { }
extension PublicConformance: InternalProtocol { }
extension PublicConformance: FilePrivateProtocol { }
extension PublicConformance: PrivateProtocol { }


private struct PrivateConformance { } // expected-provides {{PrivateConformance}}
// expected-private-member {{main.PrivateConformance.init}}

// expected-private-conformance {{main.PrivateConformance}}
extension PrivateConformance: PublicProtocol { } // expected-private-conformance {{main.PublicProtocol}}
extension PrivateConformance: InternalProtocol { } // expected-private-conformance {{main.InternalProtocol}}
extension PrivateConformance: FilePrivateProtocol { } // expected-private-conformance {{main.FilePrivateProtocol}}
extension PrivateConformance: PrivateProtocol { } // expected-private-conformance {{main.PrivateProtocol}}

