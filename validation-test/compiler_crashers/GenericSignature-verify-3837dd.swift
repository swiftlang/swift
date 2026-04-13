// {"kind":"typecheck","original":"1b6ba0fe","signature":"swift::GenericSignature::verify(llvm::ArrayRef<swift::Requirement>) const","signatureNext":"AbstractGenericSignatureRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a where b.c == Self
  protocol d: a
    protocol e: d
      struct f {
        typealias c = g
        struct g: e
        }
        typealias b = f
        func h -> some a
