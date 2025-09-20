// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %S/../Inputs/cxx-bridging-header.h -sdk %clang-importer-sdk -cxx-interoperability-mode=default -I %S/../Inputs

public func getRed() -> OuterNS.Color { OuterNS.red }
// expected-error@-1{{function cannot be declared public because its result uses an internal type}}
// expected-note@-2{{enum 'OuterNS' is imported by this file as 'internal' from bridging header}}

public func getX(point: OuterNS.MyPoint) -> Double { point.x }
// expected-error@-1{{function cannot be declared public because its parameter uses an internal type}}
// expected-note@-2{{enum 'OuterNS' is imported by this file as 'internal' from bridging header}}
