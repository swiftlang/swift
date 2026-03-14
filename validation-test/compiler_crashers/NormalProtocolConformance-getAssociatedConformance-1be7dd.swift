// {"kind":"typecheck","signature":"swift::NormalProtocolConformance::getAssociatedConformance(swift::Type, swift::ProtocolDecl*) const","signatureAssert":"Assertion failed: (!AssociatedConformances[index]), function setAssociatedConformance"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a{typealias b : IteratorProtocol} extension a{typealias Element =
                                                           b.Element} protocol c
    : a{typealias b typealias Element} protocol d
    : c{typealias Element} protocol e : d struct f : e
