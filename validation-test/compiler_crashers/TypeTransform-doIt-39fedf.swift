// {"kind":"typecheck","original":"23b7b498","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","signatureNext":"Type::transformRec"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<let b> {
  struct c {
    extension c! {
    }
  }
}
