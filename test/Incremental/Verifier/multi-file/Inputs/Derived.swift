final public class OpenSubclass: OpenBase {} // expected-provides {{OpenSubclass}} expected-cascading-superclass {{main.OpenBase}}
// expected-provides {{OpenBase}}
// expected-cascading-member {{main.OpenBase.init}}
// expected-private-member {{main.OpenBase.deinit}}
// expected-cascading-member {{main.OpenSubclass.init}}
// expected-private-member {{main.OpenSubclass.deinit}}

final public class PublicSubclass: PublicBase {} // expected-provides {{PublicSubclass}} expected-cascading-superclass {{main.PublicBase}}
// expected-provides {{PublicBase}}
// expected-cascading-member {{main.PublicBase.init}}
// expected-private-member {{main.PublicBase.deinit}}
// expected-cascading-member {{main.PublicSubclass.init}}
// expected-private-member {{main.PublicSubclass.deinit}}

final internal class InternalSubclass: InternalBase {} // expected-provides {{InternalSubclass}} expected-cascading-superclass {{main.InternalBase}}
// expected-provides {{InternalBase}}
// expected-cascading-member {{main.InternalBase.init}}
// expected-private-member {{main.InternalBase.deinit}}
// expected-cascading-member {{main.InternalSubclass.init}}
// expected-private-member {{main.InternalSubclass.deinit}}

public protocol PublicProtocol: PublicBaseProtocol {}
// expected-provides {{PublicProtocol}}
// expected-provides {{PublicBaseProtocol}}
// expected-cascading-conformance {{main.PublicBaseProtocol}}

internal protocol InternalProtocol: InternalBaseProtocol {}
// expected-provides {{InternalProtocol}}
// expected-provides {{InternalBaseProtocol}}
// expected-cascading-conformance {{main.InternalBaseProtocol}}
