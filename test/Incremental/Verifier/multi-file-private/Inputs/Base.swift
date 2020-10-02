open class OpenBase {} // expected-provides {{OpenBase}}
// expected-member {{main.OpenBase.init}}
// expected-member {{main.OpenBase.deinit}}

public class PublicBase {} // expected-provides {{PublicBase}}
// expected-member {{main.PublicBase.init}}
// expected-member {{main.PublicBase.deinit}}

internal class InternalBase {} // expected-provides {{InternalBase}}
// expected-member {{main.InternalBase.init}}
// expected-member {{main.InternalBase.deinit}}

fileprivate class FilePrivateBase {} // expected-provides {{FilePrivateBase}}
// expected-member {{main.FilePrivateBase.init}}
// expected-member {{main.FilePrivateBase.deinit}}

private class PrivateBase {} // expected-provides {{PrivateBase}}
// expected-member {{main.PrivateBase.init}}
// expected-member {{main.PrivateBase.deinit}}

final fileprivate class FilePrivateSubclass: FilePrivateBase {} // expected-provides {{FilePrivateSubclass}} expected-superclass {{main.FilePrivateBase}}
// expected-member {{main.FilePrivateSubclass.init}}
// expected-member {{main.FilePrivateSubclass.deinit}}

final private class PrivateSubclass: PrivateBase {} // expected-provides {{PrivateSubclass}} expected-superclass {{main.PrivateBase}}
// expected-member {{main.PrivateSubclass.init}}
// expected-member {{main.PrivateSubclass.deinit}}

public protocol PublicBaseProtocol {} // expected-provides {{PublicBaseProtocol}}

internal protocol InternalBaseProtocol {} // expected-provides {{InternalBaseProtocol}}

fileprivate protocol FilePrivateBaseProtocol {} // expected-provides {{FilePrivateBaseProtocol}}

private protocol PrivateBaseProtocol {} // expected-provides {{PrivateBaseProtocol}}

fileprivate protocol FilePrivateProtocol: FilePrivateBaseProtocol {} // expected-provides {{FilePrivateProtocol}} expected-conformance {{main.FilePrivateBaseProtocol}}

private protocol PrivateProtocol: PrivateBaseProtocol {} // expected-provides {{PrivateProtocol}} expected-conformance {{main.PrivateBaseProtocol}}
