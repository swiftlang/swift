// {"kind":"typecheck","original":"f0721be1","signature":"existentialConstraintJoinMeetImpl((anonymous namespace)::Operation, swift::Type, swift::Type)","signatureNext":"subtypeJoinMeetImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
type(a as Any.Type) < b as Any.Type
