open class OpenBase {} // expected-provides {{OpenBase}}
// expected-private-member {{main.OpenBase.init}}
// expected-private-member {{main.OpenBase.deinit}}

public class PublicBase {} // expected-provides {{PublicBase}}
// expected-private-member {{main.PublicBase.init}}
// expected-private-member {{main.PublicBase.deinit}}

internal class InternalBase {} // expected-provides {{InternalBase}}
// expected-private-member {{main.InternalBase.init}}
// expected-private-member {{main.InternalBase.deinit}}

fileprivate class FilePrivateBase {} // expected-provides {{FilePrivateBase}}
// expected-private-member {{main.FilePrivateBase.init}}
// expected-private-member {{main.FilePrivateBase.deinit}}

private class PrivateBase {} // expected-provides {{PrivateBase}}
// expected-private-member {{main.PrivateBase.init}}
// expected-private-member {{main.PrivateBase.deinit}}

final fileprivate class FilePrivateSubclass: FilePrivateBase {} // expected-provides {{FilePrivateSubclass}} expected-private-superclass {{main.FilePrivateBase}}
// expected-private-member {{main.FilePrivateSubclass.init}}
// expected-private-member {{main.FilePrivateSubclass.deinit}}

final private class PrivateSubclass: PrivateBase {} // expected-provides {{PrivateSubclass}} expected-private-superclass {{main.PrivateBase}}
// expected-private-member {{main.PrivateSubclass.init}}
// expected-private-member {{main.PrivateSubclass.deinit}}

public protocol PublicBaseProtocol {} // expected-provides {{PublicBaseProtocol}}

internal protocol InternalBaseProtocol {} // expected-provides {{InternalBaseProtocol}}

fileprivate protocol FilePrivateBaseProtocol {} // expected-provides {{FilePrivateBaseProtocol}}

private protocol PrivateBaseProtocol {} // expected-provides {{PrivateBaseProtocol}}

fileprivate protocol FilePrivateProtocol: FilePrivateBaseProtocol {} // expected-provides {{FilePrivateProtocol}} expected-private-conformance {{main.FilePrivateBaseProtocol}}

private protocol PrivateProtocol: PrivateBaseProtocol {} // expected-provides {{PrivateProtocol}} expected-private-conformance {{main.PrivateBaseProtocol}}
