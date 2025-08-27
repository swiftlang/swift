// {"kind":"typecheck","signature":"swift::Evaluator::diagnoseCycle(swift::ActiveRequest const&)","signatureAssert":"Assertion failed: (!ActiveDiagnostic && \"Already have an active diagnostic\"), function diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem distributed actor a{distributed...
