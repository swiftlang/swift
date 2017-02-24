#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/DeclSyntax.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

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
