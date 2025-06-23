// {"signature":"swift::Decl::getSemanticAvailableAttr(swift::AvailableAttr const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @available(*, renamed : "process0") func a () {
    async {
      a
    }
  }
}
