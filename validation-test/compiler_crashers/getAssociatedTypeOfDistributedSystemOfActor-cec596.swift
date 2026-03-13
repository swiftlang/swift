// {"kind":"typecheck","signature":"swift::getAssociatedTypeOfDistributedSystemOfActor(swift::DeclContext*, swift::Identifier)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed distributed var a{{
