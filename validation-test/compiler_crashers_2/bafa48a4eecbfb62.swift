// {"kind":"typecheck","signature":"diagnoseUnknownType(swift::TypeResolution const&, swift::Type, swift::SourceRange, swift::DeclRefTypeRepr*, swift::optionset::OptionSet<swift::NameLookupFlags, unsigned int>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class b open extension b {
  @objc c : a
