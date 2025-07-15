// {"kind":"typecheck","signature":"swift::MakeAbstractConformanceForGenericType::operator()(swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol A {}
struct C<T: A> {}
protocol E {
    associatedtype F
    func g<T>(_: C<T>) where F == T
}
struct H: E {
    typealias F = ()
    func g<T>(_: C<T>) {}
}
