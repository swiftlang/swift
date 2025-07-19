// {"signature":"swift::getAssociatedTypeOfDistributedSystemOfActor(swift::DeclContext*, swift::Identifier)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Distributed distributed var a{{
