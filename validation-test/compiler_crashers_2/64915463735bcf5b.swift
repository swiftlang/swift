// {"signature":"swift::SimpleRequest<swift::IsStaticRequest, bool (swift::FuncDecl*), (swift::RequestFlags)4>::noteCycleStep(swift::DiagnosticEngine&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem distributed actor a{distributed...
