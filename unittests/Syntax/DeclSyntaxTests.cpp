#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

#pragma mark - typealias-decl

TEST(DeclSyntaxTests, TypealiasMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<64> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Typealias =
      SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
    auto Subsequence = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
    auto ElementParam =
      SyntaxFactory::makeGenericParameter("Element", {}, {});
    auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
    auto RightAngle =
      SyntaxFactory::makeRightAngleToken({}, { Trivia::spaces(1) });
    auto GenericParams = GenericParameterClauseBuilder()
      .useLeftAngleBracket(LeftAngle)
      .useRightAngleBracket(RightAngle)
      .addParameter(None, ElementParam)
      .build();
    auto Assignment = SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

    auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

    auto GenericArgs = GenericArgumentClauseBuilder()
      .useLeftAngleBracket(LeftAngle)
      .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
      .addGenericArgument(None, ElementArg)
      .build();

    auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
    auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

    SyntaxFactory::makeTypealiasDecl(Typealias, Subsequence, GenericParams,
                                       Assignment, Array_Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasWithAPIs) {
  auto Typealias =
    SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
  auto MyCollection = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
  auto ElementParam =
    SyntaxFactory::makeGenericParameter("Element", {}, {});
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
  auto RightAngle =
    SyntaxFactory::makeRightAngleToken({}, { Trivia::spaces(1) });
  auto GenericParams = GenericParameterClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(RightAngle)
    .addParameter(None, ElementParam)
    .build();
  auto Equal = SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

  auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

  auto GenericArgs = GenericArgumentClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(None, ElementArg)
    .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankTypealiasDecl()
      .withTypeAliasKeyword(Typealias)
      .withIdentifier(MyCollection)
      .withGenericParameterClause(GenericParams)
      .withEqualToken(Equal)
      .withTypeSyntax(Array_Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(),
              "typealias MyCollection<Element> = Array<Element>");
  }
}

TEST(DeclSyntaxTests, TypealiasBuilderAPIs) {
  SmallString<64> Scratch;
  llvm::raw_svector_ostream OS(Scratch);

  auto Typealias =
    SyntaxFactory::makeTypealiasKeyword({}, { Trivia::spaces(1) });
  auto MyCollection = SyntaxFactory::makeIdentifier("MyCollection", {}, {});
  auto ElementParam =
    SyntaxFactory::makeGenericParameter("Element", {}, {});
  auto LeftAngle = SyntaxFactory::makeLeftAngleToken({}, {});
  auto RightAngle =
    SyntaxFactory::makeRightAngleToken({}, { Trivia::spaces(1) });
  auto GenericParams = GenericParameterClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(RightAngle)
    .addParameter(None, ElementParam)
    .build();
  auto Equal =
    SyntaxFactory::makeEqualToken({}, { Trivia::spaces(1) });

  auto ElementArg = SyntaxFactory::makeTypeIdentifier("Element", {}, {});

  auto GenericArgs = GenericArgumentClauseBuilder()
    .useLeftAngleBracket(LeftAngle)
    .useRightAngleBracket(SyntaxFactory::makeRightAngleToken({}, {}))
    .addGenericArgument(None, ElementArg)
    .build();

  auto Array = SyntaxFactory::makeIdentifier("Array", {}, {});
  auto Array_Int = SyntaxFactory::makeTypeIdentifier(Array, GenericArgs);

  TypeAliasDeclSyntaxBuilder()
    .useTypeAliasKeyword(Typealias)
    .useIdentifier(MyCollection)
    .useGenericParameterClause(GenericParams)
    .useEqualToken(Equal)
    .useType(Array_Int)
    .build()
    .print(OS);
  ASSERT_EQ(OS.str().str(),
            "typealias MyCollection<Element> = Array<Element>");
}

#pragma mark - parameter

FunctionParameterSyntax getCannedFunctionParameter() {
  auto ExternalName = SyntaxFactory::makeIdentifier("with", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("radius", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                               Trivia::spaces(1));
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...");
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto Sign = SyntaxFactory::makePrefixOpereator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(Sign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, Trivia::spaces(1));

  return SyntaxFactory::makeFunctionParameter(ExternalName, LocalName, Colon,
                                              Int, NoEllipsis, Equal, One,
                                              Comma);
}

TEST(DeclSyntaxTests, FunctionParameterMakeAPIs) {
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: Int = -1, ");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameter().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
}

TEST(DeclSyntaxTests, FunctionParameterGetAPIs) {
  auto ExternalName = SyntaxFactory::makeIdentifier("with", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("radius", {}, {});
  auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                                  Trivia::spaces(1));
  auto NoEllipsis = TokenSyntax::missingToken(tok::identifier, "...");
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto Sign = SyntaxFactory::makePrefixOpereator("-", {});
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(Sign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, {});

  auto Param = SyntaxFactory::makeFunctionParameter(ExternalName, LocalName,
                                                    Colon, Int, NoEllipsis,
                                                    Equal, One, Comma);

  ASSERT_EQ(ExternalName, Param.getExternalName());
  ASSERT_EQ(LocalName, Param.getLocalName());
  ASSERT_EQ(Colon, Param.getColonToken());

  auto GottenType = Param.getTypeSyntax().getValue();
  auto GottenType2 = Param.getTypeSyntax().getValue();
  ASSERT_TRUE(GottenType.hasSameIdentityAs(GottenType2));

  ASSERT_EQ(Equal, Param.getEqualToken());

  auto GottenDefaultValue = Param.getDefaultValue().getValue();
  auto GottenDefaultValue2 = Param.getDefaultValue().getValue();
  ASSERT_TRUE(GottenDefaultValue.hasSameIdentityAs(GottenDefaultValue2));

  ASSERT_EQ(Comma, Param.getTrailingComma());

  // Test that llvm::None is returned for non-token missing children:
  auto Decimated = Param
    .withTypeSyntax(llvm::None)
    .withDefaultValue(llvm::None);

  ASSERT_FALSE(Decimated.getTypeSyntax().hasValue());
  ASSERT_FALSE(Decimated.getDefaultValue().hasValue());
}

TEST(DeclSyntaxTests, FunctionParameterWithAPIs) {
  auto ExternalName = SyntaxFactory::makeIdentifier("for", {},
                                                    Trivia::spaces(1));
  auto LocalName = SyntaxFactory::makeIdentifier("integer", {}, {});
  auto Colon = SyntaxFactory::makeColonToken(Trivia::spaces(1),
                                             Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {},
                                                  Trivia::spaces(1));
  auto Equal = SyntaxFactory::makeEqualToken({}, Trivia::spaces(1));

  auto NoSign = TokenSyntax::missingToken(tok::oper_prefix, "");
  auto OneDigits = SyntaxFactory::makeIntegerLiteralToken("1", {}, {});
  auto One = SyntaxFactory::makeIntegerLiteralExpr(NoSign, OneDigits);
  auto Comma = SyntaxFactory::makeCommaToken({}, {});

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
      .withExternalName(ExternalName)
      .withLocalName(LocalName)
      .withColonToken(Colon)
      .withTypeSyntax(Int)
      .withEqualToken(Equal)
      .withDefaultValue(One)
      .withTrailingComma(Comma)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "for integer : Int = 1,");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    getCannedFunctionParameter()
      .withTypeSyntax(llvm::None)
      .withDefaultValue(llvm::None)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "with radius: = , ");
  }
}

#pragma mark - parameter-list

TEST(DeclSyntaxTests, FunctionParameterListMakeAPIs) {
  {
    SmallString<1> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionParameterList().print(OS);
    ASSERT_EQ(OS.str().str(), "");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Param = getCannedFunctionParameter();
    std::vector<FunctionParameterSyntax> Params { Param, Param, Param };
    SyntaxFactory::makeFunctionParameterList(Params).print(OS);
    ASSERT_EQ(OS.str().str(),
      "with radius: Int = -1, with radius: Int = -1, with radius: Int = -1, ");
  }
}

#pragma mark - function-signature

TEST(DeclSyntaxTests, FunctionSignatureMakeAPIs) {

}

TEST(DeclSyntaxTests, FunctionSignatureGetAPIs) {

}

TEST(DeclSyntaxTests, FunctionSignatureWithAPIs) {

}

#pragma mark - function-declaration

// TODO: Nodes
// TODO: Tests
