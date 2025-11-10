// {"signature":"swift::ProtocolConformanceRef::forAbstract(swift::Type, swift::ProtocolDecl*)"}
// RUN: not %target-swift-frontend -typecheck %s
var sixDoubles
    : Double "six has the value [\( ( sixDoubles \[]) }0 \0\0 \0 \sixDoubles)"
