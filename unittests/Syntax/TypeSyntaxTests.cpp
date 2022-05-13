#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using llvm::None;
using llvm::SmallString;

using namespace swift;
using namespace swift::syntax;

#pragma mark - type-attribute

TEST(TypeSyntaxTests, TypeAttributeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto At = Factory.makeAtSignToken("", "");
  {
    auto AutoclosureID = Factory.makeIdentifier("autoclosure", "", "");
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure =
        Factory.makeBlankAttribute().withAtSignToken(At).withAttributeName(
            AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = Factory.makeIdentifier("convention", "", "");
    auto LeftParen = Factory.makeLeftParenToken("", "");
    auto RightParen = Factory.makeRightParenToken("", "");

    auto Convention = Factory.makeBlankAttribute()
                          .withAtSignToken(At)
                          .withAttributeName(conventionID)
                          .withLeftParen(LeftParen)
                          .withRightParen(RightParen);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = Factory.makeIdentifier("c", "", "");
      Convention.withArgument(cID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = Factory.makeIdentifier("swift", "", "");
      auto swiftArgs = Factory.makeTokenList({LeftParen, swiftID, RightParen});
      Convention.withArgument(swiftID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = Factory.makeIdentifier("block", "", "");
      auto blockArgs = Factory.makeTokenList({LeftParen, blockID, RightParen});
      Convention.withArgument(blockID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = Factory.makeIdentifier("escaping", "", "");
    auto Escaping =
        Factory.makeBlankAttribute().withAtSignToken(At).withAttributeName(
            EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TypeAttributeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto At = Factory.makeAtSignToken("", "");
  {
    auto AutoclosureID = Factory.makeIdentifier("autoclosure", "", "");
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure =
        Factory.makeBlankAttribute().withAtSignToken(At).withAttributeName(
            AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = Factory.makeIdentifier("convention", "", "");
    auto LeftParen = Factory.makeLeftParenToken("", "");
    auto RightParen = Factory.makeRightParenToken("", "");

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = Factory.makeIdentifier("c", "", "");
      Factory
          .makeAttribute(At, conventionID, LeftParen, cID, RightParen,
                         llvm::None)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = Factory.makeIdentifier("swift", "", "");
      Factory
          .makeAttribute(At, conventionID, LeftParen, swiftID, RightParen,
                         llvm::None)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = Factory.makeIdentifier("block", "", "");
      Factory
          .makeAttribute(At, conventionID, LeftParen, blockID, RightParen,
                         llvm::None)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = Factory.makeIdentifier("escaping", "", "");
    auto Escaping =
        Factory.makeBlankAttribute().withAtSignToken(At).withAttributeName(
            EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TupleWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = Factory.makeVoidTupleType().withLeftParen(
        Factory.makeLeftParenToken("", ""));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = Factory.makeVoidTupleType().withLeftParen(
        Factory.makeLeftParenToken("  ", " "));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "  ( )");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = Factory.makeVoidTupleType().withRightParen(
        Factory.makeRightParenToken("    ", "\n"));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "(    )\n");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = Factory.makeVoidTupleType().withRightParen(
        Factory.makeRightParenToken("", ""));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Comma = Factory.makeCommaToken("", "");
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto Foo = Factory.makeTypeIdentifier("Foo", "", "  ");
    auto Void = Factory.makeVoidTupleType().withElements(
        Factory.makeTupleTypeElementList(
            {Factory.makeTupleTypeElement(Foo, Comma),
             Factory.makeTupleTypeElement(Foo, NoComma)}));
    Void.print(OS);
    ASSERT_EQ(OS.str().str(), "(Foo  ,Foo  )");
  }
}

TEST(TypeSyntaxTests, TupleBuilderAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder(Arena);
    Builder.useLeftParen(Factory.makeLeftParenToken("", ""));
    auto Comma = Factory.makeCommaToken("", " ");
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto IntId = Factory.makeIdentifier("Int", "", "");
    auto IntType = Factory.makeSimpleTypeIdentifier(IntId, None);
    auto Int = Factory.makeTupleTypeElement(IntType, NoComma);
    auto IntWithComma = Factory.makeTupleTypeElement(IntType, Comma);
    auto StringId = Factory.makeIdentifier("String", "", "");
    auto StringType = Factory.makeSimpleTypeIdentifier(StringId, None);
    auto String = Factory.makeTupleTypeElement(StringType, Comma);
    Builder.addElement(IntWithComma);
    Builder.addElement(String);
    Builder.addElement(Int);
    Builder.useRightParen(Factory.makeRightParenToken("", ""));

    auto TupleType = Builder.build();

    TupleType.print(OS);
    ASSERT_EQ(OS.str(), "(Int, String, Int)");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder(Arena);
    Builder.useLeftParen(Factory.makeLeftParenToken("", ""));
    auto Int = Factory.makeTypeIdentifier("Int", "", "");
    auto Comma = Factory.makeCommaToken("", " ");
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto Colon = Factory.makeColonToken("", " ");
    auto xLabel = Factory.makeIdentifier("x", {}, {});
    auto xTypeElt = Factory.makeTupleTypeElement(xLabel, Colon, Int, Comma);
    auto inout = Factory.makeInoutKeyword("", " ");
    auto yLabel = Factory.makeIdentifier("y", {}, {});
    auto yTypeElt = Factory.makeTupleTypeElement(yLabel, Colon, Int, NoComma)
                        .withInOut(inout);
    Builder.addElement(xTypeElt);
    Builder.addElement(yTypeElt);
    Builder.useRightParen(Factory.makeRightParenToken("", ""));

    auto TupleType = Builder.build();
    TupleType.print(OS);

    ASSERT_EQ(OS.str(), "(x: Int, inout y: Int)");
  }
}

TEST(TypeSyntaxTests, TupleMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = Factory.makeVoidTupleType();
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }

  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = Factory.makeTypeIdentifier("Int", "", "");
    auto Bool = Factory.makeTypeIdentifier("Bool", "", "");
    auto Comma = Factory.makeCommaToken("", " ");
    auto TupleType =
        Factory.makeTupleType(Factory.makeLeftParenToken("", ""),
                              Factory.makeTupleTypeElementList(
                                  {Factory.makeTupleTypeElement(Int, Comma),
                                   Factory.makeTupleTypeElement(Bool, Comma),
                                   Factory.makeTupleTypeElement(Int, Comma),
                                   Factory.makeTupleTypeElement(Bool, Comma),
                                   Factory.makeTupleTypeElement(Int, None)}),
                              Factory.makeRightParenToken("", ""));
    TupleType.print(OS);
    ASSERT_EQ(OS.str().str(),
              "(Int, Bool, Int, Bool, Int)");
  }
}

TEST(TypeSyntaxTests, CreateCannedTypes) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<3> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Any = Factory.makeAnyTypeIdentifier("", "");
    Any.print(OS);
    ASSERT_EQ(OS.str(), "Any");
  }

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Self = Factory.makeSelfTypeIdentifier("", "");
    Self.print(OS);
    ASSERT_EQ(OS.str(), "Self");
  }
}

TEST(TypeSyntaxTests, OptionalTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = Factory.makeTypeIdentifier("Int", "", "");
    auto Question = Factory.makePostfixQuestionMarkToken("", "");
    auto OptionalInt = Factory.makeOptionalType(Int, Question);
    OptionalInt.print(OS);
    ASSERT_EQ(OS.str(), "Int?");
  }
}

TEST(TypeSyntaxTests, OptionalTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType = Factory.makeTypeIdentifier("String", " ", "");
    Factory.makeBlankOptionalType()
        .withWrappedType(StringType)
        .withQuestionMark(Factory.makePostfixQuestionMarkToken("", ""))
        .print(OS);
    ASSERT_EQ(OS.str(), " String?");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = Factory.makeTypeIdentifier("Int", "", "");
    auto Bang = Factory.makeExclamationMarkToken("", "");
    auto IntBang = Factory.makeImplicitlyUnwrappedOptionalType(Int, Bang);
    IntBang.print(OS);
    ASSERT_EQ(OS.str(), "Int!");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType = Factory.makeTypeIdentifier("String", " ", "");
    Factory.makeBlankImplicitlyUnwrappedOptionalType()
        .withWrappedType(StringType)
        .withExclamationMark(Factory.makeExclamationMarkToken("", ""))
        .print(OS);
    ASSERT_EQ(OS.str(), " String!");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = Factory.makeTypeIdentifier("T", "", "");
    auto Dot = Factory.makePeriodToken("", "");
    auto Type = Factory.makeTypeToken("", "");
    Factory.makeMetatypeType(Int, Dot, Type).print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Int = Factory.makeTypeIdentifier("T", "", "");
  auto Dot = Factory.makePeriodToken("", "");
  auto Type = Factory.makeTypeToken("", "");
  auto Protocol = Factory.makeProtocolToken("", "");

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankMetatypeType()
        .withBaseType(Int)
        .withPeriod(Dot)
        .withTypeOrProtocol(Type)
        .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankMetatypeType()
        .withBaseType(Int)
        .withPeriod(Dot)
        .withTypeOrProtocol(Protocol)
        .print(OS);
    ASSERT_EQ(OS.str(), "T.Protocol");
  }

#ifndef NDEBUG
  ASSERT_DEATH(
      {
        Factory.makeBlankMetatypeType()
            .withBaseType(Int)
            .withPeriod(Dot)
            .withTypeOrProtocol(Factory.makeIdentifier("WRONG", "", ""));
      },
      "");
#endif
}

TEST(TypeSyntaxTests, ArrayTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = Factory.makeLeftSquareBracketToken("", "");
    auto RightSquare = Factory.makeRightSquareBracketToken("", "");
    auto Double = Factory.makeTypeIdentifier("Double", "", "");
    Factory.makeBlankArrayType()
        .withLeftSquareBracket(LeftSquare)
        .withElementType(Double)
        .withRightSquareBracket(RightSquare)
        .print(OS);
    ASSERT_EQ(OS.str(), "[Double]");
  }
}

TEST(TypeSyntaxTests, ArrayTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = Factory.makeLeftSquareBracketToken("", "");
    auto RightSquare = Factory.makeRightSquareBracketToken("", "");
    auto Void = Factory.makeVoidTupleType();
    Factory.makeArrayType(LeftSquare, Void, RightSquare).print(OS);
    ASSERT_EQ(OS.str(), "[()]");
  }
}

TEST(TypeSyntaxTests, DictionaryTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto LeftSquare = Factory.makeLeftSquareBracketToken("", "");
    auto RightSquare = Factory.makeRightSquareBracketToken("", "");
    auto Key = Factory.makeTypeIdentifier("String", "", " ");
    auto Value = Factory.makeTypeIdentifier("Int", "", "");
    auto Colon = Factory.makeColonToken("", " ");
    Factory.makeBlankDictionaryType()
        .withLeftSquareBracket(LeftSquare)
        .withKeyType(Key)
        .withColon(Colon)
        .withValueType(Value)
        .withRightSquareBracket(RightSquare)
        .print(OS);

    ASSERT_EQ(OS.str(), "[String : Int]");
  }
}

TEST(TypeSyntaxTests, DictionaryTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = Factory.makeLeftSquareBracketToken("", "");
    auto RightSquare = Factory.makeRightSquareBracketToken("", "");
    auto Key = Factory.makeTypeIdentifier("String", "", " ");
    auto Value = Factory.makeTypeIdentifier("Int", "", "");
    auto Colon = Factory.makeColonToken("", " ");
    Factory.makeDictionaryType(LeftSquare, Key, Colon, Value, RightSquare)
        .print(OS);
    ASSERT_EQ(OS.str(), "[String : Int]");
  }
}

TEST(TypeSyntaxTests, FunctionTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto Colon = Factory.makeColonToken("", " ");
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto RightParen = Factory.makeRightParenToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", "");
  auto IntArg = Factory.makeBlankTupleTypeElement().withType(Int);
  auto Async = Factory.makeContextualKeyword("async", "", " ");
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Rethrows = Factory.makeRethrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = Factory.makeIdentifier("x", "", "");
    auto y = Factory.makeIdentifier("y", "", "");
    auto xArg = Factory.makeBlankTupleTypeElement()
                    .withName(x)
                    .withColon(Colon)
                    .withType(Int)
                    .withTrailingComma(Comma);
    auto yArg = Factory.makeBlankTupleTypeElement()
                    .withName(y)
                    .withColon(Colon)
                    .withType(Int);

    auto TypeList = Factory.makeTupleTypeElementList({xArg, yArg});
    Factory
        .makeFunctionType(LeftParen, TypeList, RightParen, Async, Throws, Arrow,
                          Int)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) async throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = Factory.makeIdentifier("x", "", "");
    auto y = Factory.makeIdentifier("y", "", "");
    auto xArg = Factory.makeBlankTupleTypeElement()
                    .withName(x)
                    .withColon(Colon)
                    .withType(Int)
                    .withTrailingComma(Comma);
    auto yArg = Factory.makeBlankTupleTypeElement()
                    .withName(y)
                    .withColon(Colon)
                    .withType(Int);

    auto TypeList = Factory.makeTupleTypeElementList({xArg, yArg});
    Factory
        .makeFunctionType(LeftParen, TypeList, RightParen, None, Throws, Arrow,
                          Int)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto TypeList = Factory.makeTupleTypeElementList(
        {IntArg.withTrailingComma(Comma), IntArg});
    Factory
        .makeFunctionType(LeftParen, TypeList, RightParen, None, Rethrows,
                          Arrow, Int)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto TypeList = Factory.makeBlankTupleTypeElementList();
    auto Void = Factory.makeVoidTupleType();
    Factory
        .makeFunctionType(
            LeftParen, TypeList, RightParen, None,
            TokenSyntax::missingToken(tok::kw_throws, "throws", Arena), Arrow,
            Void)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}

TEST(TypeSyntaxTests, FunctionTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto RightParen = Factory.makeRightParenToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", "");
  auto IntArg = Factory.makeTupleTypeElement(None, None, None, None, Int, None,
                                             None, None);
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Rethrows = Factory.makeRethrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto x = Factory.makeIdentifier("x", "", "");
    auto y = Factory.makeIdentifier("y", "", "");
    auto Colon = Factory.makeColonToken("", " ");
    auto xArg = Factory.makeTupleTypeElement(None, x, None, Colon, Int, None,
                                             None, Comma);
    auto yArg = Factory.makeTupleTypeElement(None, y, None, Colon, Int, None,
                                             None, None);

    Factory.makeBlankFunctionType()
        .withLeftParen(LeftParen)
        .addArgument(xArg)
        .addArgument(yArg)
        .withRightParen(RightParen)
        .withThrowsOrRethrowsKeyword(Throws)
        .withArrow(Arrow)
        .withReturnType(Int)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");


  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    Factory.makeBlankFunctionType()
        .withLeftParen(LeftParen)
        .withRightParen(RightParen)
        .addArgument(IntArg.withTrailingComma(Comma))
        .addArgument(IntArg)
        .withThrowsOrRethrowsKeyword(Rethrows)
        .withArrow(Arrow)
        .withReturnType(Int)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto Void = Factory.makeVoidTupleType();
    Factory.makeBlankFunctionType()
        .withLeftParen(LeftParen)
        .withRightParen(RightParen)
        .withArrow(Arrow)
        .withReturnType(Void)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}

TEST(TypeSyntaxTests, FunctionTypeBuilderAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  SyntaxFactory Factory(Arena);
  auto Comma = Factory.makeCommaToken("", " ");
  auto Int = Factory.makeTypeIdentifier("Int", "", "");
  auto LeftParen = Factory.makeLeftParenToken("", "");
  auto RightParen = Factory.makeRightParenToken("", " ");
  auto Throws = Factory.makeThrowsKeyword("", " ");
  auto Rethrows = Factory.makeRethrowsKeyword("", " ");
  auto Arrow = Factory.makeArrowToken("", " ");

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder(Arena);
    auto x = Factory.makeIdentifier("x", "", "");
    auto y = Factory.makeIdentifier("y", "", "");
    auto Colon = Factory.makeColonToken("", " ");
    auto xArg = Factory.makeTupleTypeElement(None, x, None, Colon, Int, None,
                                             None, Comma);
    auto yArg = Factory.makeTupleTypeElement(None, y, None, Colon, Int, None,
                                             None, None);

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addArgument(xArg)
      .addArgument(yArg)
      .useThrowsOrRethrowsKeyword(Throws)
      .useArrow(Arrow)
      .useReturnType(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder(Arena);
    auto IntArg = Factory.makeTupleTypeElement(None, None, None, None, Int,
                                               None, None, None);
    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addArgument(IntArg.withTrailingComma(Comma))
      .addArgument(IntArg)
      .useThrowsOrRethrowsKeyword(Rethrows)
      .useArrow(Arrow)
      .useReturnType(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder(Arena);
    auto Void = Factory.makeVoidTupleType();

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .useArrow(Arrow)
      .useReturnType(Void);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}
