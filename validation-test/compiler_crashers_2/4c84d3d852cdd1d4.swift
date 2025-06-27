// {"signature":"swift::ProtocolConformanceRef::forAbstract(swift::Type, swift::ProtocolDecl*)"}
// RUN: env DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
// REQUIRES: target-same-as-host
// REQUIRES: no_asan
var sixDoubles
    : Double "six has the value [\( ( sixDoubles \[]) }0 \0\0 \0 \sixDoubles)"
