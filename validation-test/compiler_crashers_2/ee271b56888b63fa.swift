// {"signature":"swift::InFlightSubstitution::substType(swift::SubstitutableType*, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a< each b > = ( < struct c< each d {
           typealias e< each b > = (repeat a< each d, each b >
          struct f typealias 1 = c .e<>
