final public class OpenSubclass: OpenBase {} // expected-provides {{OpenSubclass}} expected-private-superclass {{main.OpenBase}}
// expected-provides {{OpenBase}}
// expected-private-member {{main.OpenBase.init}}
// expected-private-member {{main.OpenBase.deinit}}
// expected-private-member {{main.OpenSubclass.init}}
// expected-private-member {{main.OpenSubclass.deinit}}

final public class PublicSubclass: PublicBase {} // expected-provides {{PublicSubclass}} expected-private-superclass {{main.PublicBase}}
// expected-provides {{PublicBase}}
// expected-private-member {{main.PublicBase.init}}
// expected-private-member {{main.PublicBase.deinit}}
// expected-private-member {{main.PublicSubclass.init}}
// expected-private-member {{main.PublicSubclass.deinit}}

final internal class InternalSubclass: InternalBase {} // expected-provides {{InternalSubclass}} expected-private-superclass {{main.InternalBase}}
// expected-provides {{InternalBase}}
// expected-private-member {{main.InternalBase.init}}
// expected-private-member {{main.InternalBase.deinit}}
// expected-private-member {{main.InternalSubclass.init}}
// expected-private-member {{main.InternalSubclass.deinit}}

public protocol PublicProtocol: PublicBaseProtocol {}
// expected-provides {{PublicProtocol}}
// expected-provides {{PublicBaseProtocol}}
// expected-private-conformance {{main.PublicBaseProtocol}}

internal protocol InternalProtocol: InternalBaseProtocol {}
// expected-provides {{InternalProtocol}}
// expected-provides {{InternalBaseProtocol}}
// expected-private-conformance {{main.InternalBaseProtocol}}
