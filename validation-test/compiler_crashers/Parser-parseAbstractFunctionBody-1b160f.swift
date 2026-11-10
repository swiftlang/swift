// {"kind":"typecheck","signature":"swift::Parser::parseAbstractFunctionBody(swift::AbstractFunctionDecl*, swift::optionset::OptionSet<swift::Parser::ParseDeclFlags, unsigned short>)","signatureAssert":"Assertion failed: (!Flags.contains(PD_StubOnly) && \"stub-only should parse body immediately\"), function parseAbstractFunctionBody","signatureNext":"Parser::parseDeclFunc"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(func a {
