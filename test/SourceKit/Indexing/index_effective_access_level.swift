// RUN: %sourcekitd-test -req=index %s -- -Xfrontend -serialize-diagnostics-path -Xfrontend %t.dia %s | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response

public enum PublicEn {
    case a
    case b
}

enum InternalEn {
    case a
    case b
}

fileprivate enum FilePrivateEn {
    case a
    case b
}

private enum PrivateEn {
    case a
    case b
}

extension PublicEn {
    public func puFoo() {}
}

public extension PublicEn {
    func puFooFromPublicExtension() {}
}

extension InternalEn {
    func foo() {}
}

extension FilePrivateEn {
    fileprivate func flPrFoo() {}
}

fileprivate extension FilePrivateEn {
    func flPrFooFromFilePrivateExtension() {}
}

extension PrivateEn {
    private func prFoo() {}
}

private extension PrivateEn {
    func prFooFromPrivateExtension() {}
}

private struct ScopeReducerStruct {
    public func a() {}
    func b() {}
    fileprivate func c() {}
    private func d() {}
}

public struct ScopeKeeperStruct {
    public func a() {}
    func b() {}
    fileprivate func c() {}
    private func d() {}
}

struct PartialScopeReducerStruct {
    public func a() {}
    func b() {}
    fileprivate func c() {}
    private func d() {}
}
