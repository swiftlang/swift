final public class OpenSubclass: OpenBase {} // expected-provides {{OpenSubclass}} expected-superclass {{main.OpenBase}}
// expected-provides {{OpenBase}}
// expected-member {{main.OpenBase.init}}
// expected-member {{main.OpenBase.deinit}}
// expected-member {{main.OpenSubclass.init}}
// expected-member {{main.OpenSubclass.deinit}}

final public class PublicSubclass: PublicBase {} // expected-provides {{PublicSubclass}} expected-superclass {{main.PublicBase}}
// expected-provides {{PublicBase}}
// expected-member {{main.PublicBase.init}}
// expected-member {{main.PublicBase.deinit}}
// expected-member {{main.PublicSubclass.init}}
// expected-member {{main.PublicSubclass.deinit}}

final internal class InternalSubclass: InternalBase {} // expected-provides {{InternalSubclass}} expected-superclass {{main.InternalBase}}
// expected-provides {{InternalBase}}
// expected-member {{main.InternalBase.init}}
// expected-member {{main.InternalBase.deinit}}
// expected-member {{main.InternalSubclass.init}}
// expected-member {{main.InternalSubclass.deinit}}

public protocol PublicProtocol: PublicBaseProtocol {}
// expected-provides {{PublicProtocol}}
// expected-provides {{PublicBaseProtocol}}
// expected-conformance {{main.PublicBaseProtocol}}

internal protocol InternalProtocol: InternalBaseProtocol {}
// expected-provides {{InternalProtocol}}
// expected-provides {{InternalBaseProtocol}}
// expected-conformance {{main.InternalBaseProtocol}}
