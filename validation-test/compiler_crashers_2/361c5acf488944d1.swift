// {"kind":"typecheck","signature":"swift::InFlightSubstitution::substType(swift::SubstitutableType*, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<b, c, d> = () struct e < each b {
  typealias f<each c, d> = (repeat a<each b, each c, d>)struct g < each c {
          typealias h< each d > = (repeat f< repeat each c, each d >
  struct i typealias j< each d > =
      e< i >.g< repeat each b >.h< repeat each d >
