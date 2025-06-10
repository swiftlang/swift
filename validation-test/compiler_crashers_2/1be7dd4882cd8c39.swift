// {"signature":"swift::RootProtocolConformance::getWitness(swift::ValueDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{typealias b : IteratorProtocol} extension a{typealias Element =
                                                           b.Element} protocol c
    : a{typealias b typealias Element} protocol d
    : c{typealias Element} protocol e : d struct f : e
