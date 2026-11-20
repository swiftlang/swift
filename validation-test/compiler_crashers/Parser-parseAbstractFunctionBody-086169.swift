// {"kind":"typecheck","original":"a60a134e","signature":"swift::Parser::parseAbstractFunctionBody(swift::AbstractFunctionDecl*, swift::optionset::OptionSet<swift::Parser::ParseDeclFlags, unsigned short>)","signatureAssert":"Assertion failed: (!Flags.contains(PD_StubOnly) && \"stub-only should parse body immediately\"), function parseAbstractFunctionBody","signatureNext":"Parser::parseGetSet"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@abi(
  let a {
    {
    }
  }
)
<#declaration#>
