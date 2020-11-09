public enum Enum { case one } // expected-note {{type declared here}}
public enum GenericEnum<T> { case one(Int) } // expected-note {{type declared here}}

public struct Struct {} // expected-note {{type declared here}}
public struct GenericStruct<T> {} // expected-note {{type declared here}}

