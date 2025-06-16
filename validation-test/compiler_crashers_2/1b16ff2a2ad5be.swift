// {"signature":"swift::Parser::parseAbstractFunctionBody(swift::AbstractFunctionDecl*, swift::optionset::OptionSet<swift::Parser::ParseDeclFlags, unsigned short>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a {
