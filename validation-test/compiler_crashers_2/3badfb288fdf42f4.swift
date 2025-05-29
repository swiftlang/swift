// {"signature":"(anonymous namespace)::TypeSubstituter::transformDependentMemberType(swift::DependentMemberType*, swift::TypePosition)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: OS=macosx
import Combine extension Publishers.Share where Upstream == {a <b , c > where Upstream == Publishers.FlatMap <b, c>
