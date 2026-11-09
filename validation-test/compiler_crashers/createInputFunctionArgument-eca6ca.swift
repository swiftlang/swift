// {"kind":"emit-silgen","original":"2f004000","signature":"createInputFunctionArgument(swift::Lowering::SILGenBuilder&, swift::SILType, swift::SILLocation, swift::ValueDecl*, bool, swift::LifetimeAnnotation, bool, bool, bool)","signatureNext":"Lowering::SILGenBuilder::createInputFunctionArgument"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 6.0, *)
enum a: Error {
  struct b {
    var c: [(b) throws(a) -> String]
    mutating func d<e>(
      f: e
    ) {
      c.append { h throws(a) in
        let g = f
      }
    }
  }
}
