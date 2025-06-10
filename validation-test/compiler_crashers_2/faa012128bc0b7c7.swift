// {"signature":"swift::TypeBase::getContextSubstitutions(swift::DeclContext const*, swift::GenericEnvironment*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{                      $0?={
