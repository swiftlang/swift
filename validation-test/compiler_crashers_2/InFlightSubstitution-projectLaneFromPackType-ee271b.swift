// {"kind":"typecheck","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (index < substPackType->getNumElements() && \"replacement for pack parameter did not have the right \" \"size for expansion\"), function projectLaneFromPackType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a< each b > = ( < struct c< each d {
           typealias e< each b > = (repeat a< each d, each b >
          struct f typealias 1 = c .e<>
