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
  auto At = SyntaxFactory::makeAtSignToken("", "", Arena);
  {
    auto AutoclosureID =
        SyntaxFactory::makeIdentifier("autoclosure", "", "", Arena);
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure = SyntaxFactory::makeBlankAttribute(Arena)
                           .withAtSignToken(At)
                           .withAttributeName(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID =
        SyntaxFactory::makeIdentifier("convention", "", "", Arena);
    auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
    auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

    auto Convention = SyntaxFactory::makeBlankAttribute(Arena)
                          .withAtSignToken(At)
                          .withAttributeName(conventionID)
                          .withLeftParen(LeftParen)
                          .withRightParen(RightParen);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", "", "", Arena);
      Convention.withArgument(cID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", "", "", Arena);
      auto swiftArgs =
          SyntaxFactory::makeTokenList({LeftParen, swiftID, RightParen}, Arena);
      Convention.withArgument(swiftID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", "", "", Arena);
      auto blockArgs =
          SyntaxFactory::makeTokenList({LeftParen, blockID, RightParen}, Arena);
      Convention.withArgument(blockID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", "", "", Arena);
    auto Escaping = SyntaxFactory::makeBlankAttribute(Arena)
                        .withAtSignToken(At)
                        .withAttributeName(EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TypeAttributeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto At = SyntaxFactory::makeAtSignToken("", "", Arena);
  {
    auto AutoclosureID =
        SyntaxFactory::makeIdentifier("autoclosure", "", "", Arena);
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure = SyntaxFactory::makeBlankAttribute(Arena)
                           .withAtSignToken(At)
                           .withAttributeName(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID =
        SyntaxFactory::makeIdentifier("convention", "", "", Arena);
    auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
    auto RightParen = SyntaxFactory::makeRightParenToken("", "", Arena);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", "", "", Arena);
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, cID, RightParen,
                                   llvm::None, Arena)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", "", "", Arena);
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, swiftID,
                                   RightParen, llvm::None, Arena)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", "", "", Arena);
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, blockID,
                                   RightParen, llvm::None, Arena)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", "", "", Arena);
    auto Escaping = SyntaxFactory::makeBlankAttribute(Arena)
                        .withAtSignToken(At)
                        .withAttributeName(EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TupleWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType(Arena).withLeftParen(
        SyntaxFactory::makeLeftParenToken("", "", Arena));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType(Arena).withLeftParen(
        SyntaxFactory::makeLeftParenToken("  ", " ", Arena));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "  ( )");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType(Arena).withRightParen(
        SyntaxFactory::makeRightParenToken("    ", "\n", Arena));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "(    )\n");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType(Arena).withRightParen(
        SyntaxFactory::makeRightParenToken("", "", Arena));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Comma = SyntaxFactory::makeCommaToken("", "", Arena);
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto Foo = SyntaxFactory::makeTypeIdentifier("Foo", "", "  ", Arena);
    auto Void = SyntaxFactory::makeVoidTupleType(Arena).withElements(
        SyntaxFactory::makeTupleTypeElementList(
            {SyntaxFactory::makeTupleTypeElement(Foo, Comma, Arena),
             SyntaxFactory::makeTupleTypeElement(Foo, NoComma, Arena)},
            Arena));
    Void.print(OS);
    ASSERT_EQ(OS.str().str(), "(Foo  ,Foo  )");
  }
}

TEST(TypeSyntaxTests, TupleBuilderAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder(Arena);
    Builder.useLeftParen(SyntaxFactory::makeLeftParenToken("", "", Arena));
    auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto IntId = SyntaxFactory::makeIdentifier("Int", "", "", Arena);
    auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(IntId, None, Arena);
    auto Int = SyntaxFactory::makeTupleTypeElement(IntType, NoComma, Arena);
    auto IntWithComma =
        SyntaxFactory::makeTupleTypeElement(IntType, Comma, Arena);
    auto StringId = SyntaxFactory::makeIdentifier("String", "", "", Arena);
    auto StringType =
        SyntaxFactory::makeSimpleTypeIdentifier(StringId, None, Arena);
    auto String = SyntaxFactory::makeTupleTypeElement(StringType, Comma, Arena);
    Builder.addElement(IntWithComma);
    Builder.addElement(String);
    Builder.addElement(Int);
    Builder.useRightParen(SyntaxFactory::makeRightParenToken("", "", Arena));

    auto TupleType = Builder.build();

    TupleType.print(OS);
    ASSERT_EQ(OS.str(), "(Int, String, Int)");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder(Arena);
    Builder.useLeftParen(SyntaxFactory::makeLeftParenToken("", "", Arena));
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
    auto NoComma = TokenSyntax::missingToken(tok::comma, ",", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    auto xLabel = SyntaxFactory::makeIdentifier("x", {}, {}, Arena);
    auto xTypeElt =
        SyntaxFactory::makeTupleTypeElement(xLabel, Colon, Int, Comma, Arena);
    auto inout = SyntaxFactory::makeInoutKeyword("", " ", Arena);
    auto yLabel = SyntaxFactory::makeIdentifier("y", {}, {}, Arena);
    auto yTypeElt =
        SyntaxFactory::makeTupleTypeElement(yLabel, Colon, Int, NoComma, Arena)
            .withInOut(inout);
    Builder.addElement(xTypeElt);
    Builder.addElement(yTypeElt);
    Builder.useRightParen(SyntaxFactory::makeRightParenToken("", "", Arena));

    auto TupleType = Builder.build();
    TupleType.print(OS);

    ASSERT_EQ(OS.str(), "(x: Int, inout y: Int)");
  }
}

TEST(TypeSyntaxTests, TupleMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType(Arena);
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }

  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Bool = SyntaxFactory::makeTypeIdentifier("Bool", "", "", Arena);
    auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
    auto TupleType = SyntaxFactory::makeTupleType(
        SyntaxFactory::makeLeftParenToken("", "", Arena),
        SyntaxFactory::makeTupleTypeElementList(
            {SyntaxFactory::makeTupleTypeElement(Int, Comma, Arena),
             SyntaxFactory::makeTupleTypeElement(Bool, Comma, Arena),
             SyntaxFactory::makeTupleTypeElement(Int, Comma, Arena),
             SyntaxFactory::makeTupleTypeElement(Bool, Comma, Arena),
             SyntaxFactory::makeTupleTypeElement(Int, None, Arena)},
            Arena),
        SyntaxFactory::makeRightParenToken("", "", Arena), Arena);
    TupleType.print(OS);
    ASSERT_EQ(OS.str().str(),
              "(Int, Bool, Int, Bool, Int)");
  }
}

TEST(TypeSyntaxTests, CreateCannedTypes) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<3> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Any = SyntaxFactory::makeAnyTypeIdentifier("", "", Arena);
    Any.print(OS);
    ASSERT_EQ(OS.str(), "Any");
  }

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Self = SyntaxFactory::makeSelfTypeIdentifier("", "", Arena);
    Self.print(OS);
    ASSERT_EQ(OS.str(), "Self");
  }
}

TEST(TypeSyntaxTests, OptionalTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Question = SyntaxFactory::makePostfixQuestionMarkToken("", "", Arena);
    auto OptionalInt = SyntaxFactory::makeOptionalType(Int, Question, Arena);
    OptionalInt.print(OS);
    ASSERT_EQ(OS.str(), "Int?");
  }
}

TEST(TypeSyntaxTests, OptionalTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType =
        SyntaxFactory::makeTypeIdentifier("String", " ", "", Arena);
    SyntaxFactory::makeBlankOptionalType(Arena)
        .withWrappedType(StringType)
        .withQuestionMark(
            SyntaxFactory::makePostfixQuestionMarkToken("", "", Arena))
        .print(OS);
    ASSERT_EQ(OS.str(), " String?");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Bang = SyntaxFactory::makeExclamationMarkToken("", "", Arena);
    auto IntBang =
        SyntaxFactory::makeImplicitlyUnwrappedOptionalType(Int, Bang, Arena);
    IntBang.print(OS);
    ASSERT_EQ(OS.str(), "Int!");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType =
        SyntaxFactory::makeTypeIdentifier("String", " ", "", Arena);
    SyntaxFactory::makeBlankImplicitlyUnwrappedOptionalType(Arena)
        .withWrappedType(StringType)
        .withExclamationMark(
            SyntaxFactory::makeExclamationMarkToken("", "", Arena))
        .print(OS);
    ASSERT_EQ(OS.str(), " String!");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("T", "", "", Arena);
    auto Dot = SyntaxFactory::makePeriodToken("", "", Arena);
    auto Type = SyntaxFactory::makeTypeToken("", "", Arena);
    SyntaxFactory::makeMetatypeType(Int, Dot, Type, Arena).print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Int = SyntaxFactory::makeTypeIdentifier("T", "", "", Arena);
  auto Dot = SyntaxFactory::makePeriodToken("", "", Arena);
  auto Type = SyntaxFactory::makeTypeToken("", "", Arena);
  auto Protocol = SyntaxFactory::makeProtocolToken("", "", Arena);

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType(Arena)
        .withBaseType(Int)
        .withPeriod(Dot)
        .withTypeOrProtocol(Type)
        .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType(Arena)
        .withBaseType(Int)
        .withPeriod(Dot)
        .withTypeOrProtocol(Protocol)
        .print(OS);
    ASSERT_EQ(OS.str(), "T.Protocol");
  }

#ifndef NDEBUG
  ASSERT_DEATH(
      {
        SyntaxFactory::makeBlankMetatypeType(Arena)
            .withBaseType(Int)
            .withPeriod(Dot)
            .withTypeOrProtocol(
                SyntaxFactory::makeIdentifier("WRONG", "", "", Arena));
      },
      "");
#endif
}

TEST(TypeSyntaxTests, ArrayTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken("", "", Arena);
    auto RightSquare =
        SyntaxFactory::makeRightSquareBracketToken("", "", Arena);
    auto Double = SyntaxFactory::makeTypeIdentifier("Double", "", "", Arena);
    SyntaxFactory::makeBlankArrayType(Arena)
        .withLeftSquareBracket(LeftSquare)
        .withElementType(Double)
        .withRightSquareBracket(RightSquare)
        .print(OS);
    ASSERT_EQ(OS.str(), "[Double]");
  }
}

TEST(TypeSyntaxTests, ArrayTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken("", "", Arena);
    auto RightSquare =
        SyntaxFactory::makeRightSquareBracketToken("", "", Arena);
    auto Void = SyntaxFactory::makeVoidTupleType(Arena);
    SyntaxFactory::makeArrayType(LeftSquare, Void, RightSquare, Arena)
        .print(OS);
    ASSERT_EQ(OS.str(), "[()]");
  }
}

TEST(TypeSyntaxTests, DictionaryTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken("", "", Arena);
    auto RightSquare =
        SyntaxFactory::makeRightSquareBracketToken("", "", Arena);
    auto Key = SyntaxFactory::makeTypeIdentifier("String", "", " ", Arena);
    auto Value = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    SyntaxFactory::makeBlankDictionaryType(Arena)
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
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken("", "", Arena);
    auto RightSquare =
        SyntaxFactory::makeRightSquareBracketToken("", "", Arena);
    auto Key = SyntaxFactory::makeTypeIdentifier("String", "", " ", Arena);
    auto Value = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    SyntaxFactory::makeDictionaryType(LeftSquare, Key, Colon, Value,
                                      RightSquare, Arena)
        .print(OS);
    ASSERT_EQ(OS.str(), "[String : Int]");
  }
}

TEST(TypeSyntaxTests, FunctionTypeMakeAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
  auto IntArg = SyntaxFactory::makeBlankTupleTypeElement(Arena).withType(Int);
  auto Async = SyntaxFactory::makeIdentifier("async", "", " ", Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = SyntaxFactory::makeIdentifier("x", "", "", Arena);
    auto y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
    auto xArg = SyntaxFactory::makeBlankTupleTypeElement(Arena)
                    .withName(x)
                    .withColon(Colon)
                    .withType(Int)
                    .withTrailingComma(Comma);
    auto yArg = SyntaxFactory::makeBlankTupleTypeElement(Arena)
                    .withName(y)
                    .withColon(Colon)
                    .withType(Int);

    auto TypeList =
        SyntaxFactory::makeTupleTypeElementList({xArg, yArg}, Arena);
    SyntaxFactory::makeFunctionType(LeftParen, TypeList, RightParen, Async,
                                    Throws, Arrow, Int, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) async throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = SyntaxFactory::makeIdentifier("x", "", "", Arena);
    auto y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
    auto xArg = SyntaxFactory::makeBlankTupleTypeElement(Arena)
                    .withName(x)
                    .withColon(Colon)
                    .withType(Int)
                    .withTrailingComma(Comma);
    auto yArg = SyntaxFactory::makeBlankTupleTypeElement(Arena)
                    .withName(y)
                    .withColon(Colon)
                    .withType(Int);

    auto TypeList =
        SyntaxFactory::makeTupleTypeElementList({xArg, yArg}, Arena);
    SyntaxFactory::makeFunctionType(LeftParen, TypeList, RightParen, None,
                                    Throws, Arrow, Int, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto TypeList = SyntaxFactory::makeTupleTypeElementList(
        {IntArg.withTrailingComma(Comma), IntArg}, Arena);
    SyntaxFactory::makeFunctionType(LeftParen, TypeList, RightParen, None,
                                    Rethrows, Arrow, Int, Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto TypeList = SyntaxFactory::makeBlankTupleTypeElementList(Arena);
    auto Void = SyntaxFactory::makeVoidTupleType(Arena);
    SyntaxFactory::makeFunctionType(
        LeftParen, TypeList, RightParen, None,
        TokenSyntax::missingToken(tok::kw_throws, "throws", Arena), Arrow, Void,
        Arena)
        .print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}

TEST(TypeSyntaxTests, FunctionTypeWithAPIs) {
  RC<SyntaxArena> Arena = SyntaxArena::make();
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
  auto IntArg = SyntaxFactory::makeTupleTypeElement(None, None, None, None, Int,
                                                    None, None, None, Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto x = SyntaxFactory::makeIdentifier("x", "", "", Arena);
    auto y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    auto xArg = SyntaxFactory::makeTupleTypeElement(None, x, None, Colon, Int,
                                                    None, None, Comma, Arena);
    auto yArg = SyntaxFactory::makeTupleTypeElement(None, y, None, Colon, Int,
                                                    None, None, None, Arena);

    SyntaxFactory::makeBlankFunctionType(Arena)
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
    SyntaxFactory::makeBlankFunctionType(Arena)
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

    auto Void = SyntaxFactory::makeVoidTupleType(Arena);
    SyntaxFactory::makeBlankFunctionType(Arena)
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
  auto Comma = SyntaxFactory::makeCommaToken("", " ", Arena);
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", "", "", Arena);
  auto LeftParen = SyntaxFactory::makeLeftParenToken("", "", Arena);
  auto RightParen = SyntaxFactory::makeRightParenToken("", " ", Arena);
  auto Throws = SyntaxFactory::makeThrowsKeyword("", " ", Arena);
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword("", " ", Arena);
  auto Arrow = SyntaxFactory::makeArrowToken("", " ", Arena);

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder(Arena);
    auto x = SyntaxFactory::makeIdentifier("x", "", "", Arena);
    auto y = SyntaxFactory::makeIdentifier("y", "", "", Arena);
    auto Colon = SyntaxFactory::makeColonToken("", " ", Arena);
    auto xArg = SyntaxFactory::makeTupleTypeElement(None, x, None, Colon, Int,
                                                    None, None, Comma, Arena);
    auto yArg = SyntaxFactory::makeTupleTypeElement(None, y, None, Colon, Int,
                                                    None, None, None, Arena);

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
    auto IntArg = SyntaxFactory::makeTupleTypeElement(
        None, None, None, None, Int, None, None, None, Arena);
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
    auto Void = SyntaxFactory::makeVoidTupleType(Arena);

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .useArrow(Arrow)
      .useReturnType(Void);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}
