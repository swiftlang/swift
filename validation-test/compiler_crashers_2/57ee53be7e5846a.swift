// {"signature":"substPrefixType(swift::Type, unsigned int, swift::Type, swift::GenericSignature)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  protocol a{associatedtype b} extension a {
    extension a {
      struct c {
        d : b
