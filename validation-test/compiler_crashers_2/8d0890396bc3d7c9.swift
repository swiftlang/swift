// {"kind":"emit-silgen","original":"be1cdd08","signature":"swift::SILVisitorBase<(anonymous namespace)::SILVerifier, void>::visitSILBasicBlock(swift::SILBasicBlock*)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
import Foundation
var a = 1
[a] as [NSNumber? ]
