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
  auto At = SyntaxFactory::makeAtSignToken({}, {});
  {
    auto AutoclosureID = SyntaxFactory::makeIdentifier("autoclosure", {}, {});
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withIdentifier(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = SyntaxFactory::makeIdentifier("convention", {}, {});
    auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
    auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

    auto Convention = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withIdentifier(conventionID)
      .withLeftParen(LeftParen)
      .withRightParen(RightParen);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", {}, {});
      auto cArgs = SyntaxFactory::makeTokenList({cID});
      Convention.withBalancedTokens(cArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      auto swiftArgs = SyntaxFactory::makeTokenList({swiftID});
      Convention.withBalancedTokens(swiftArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      auto blockArgs = SyntaxFactory::makeTokenList({blockID});
      Convention.withBalancedTokens(blockArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", {}, {});
    auto Escaping = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withIdentifier(EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TypeAttributeMakeAPIs) {
  auto At = SyntaxFactory::makeAtSignToken({}, {});
  {
    auto AutoclosureID = SyntaxFactory::makeIdentifier("autoclosure", {}, {});
    SmallString<24> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Autoclosure = SyntaxFactory::makeBlankAttribute()
    .withAtSignToken(At)
    .withIdentifier(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = SyntaxFactory::makeIdentifier("convention", {}, {});
    auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
    auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", {}, {});
      auto cArgs = SyntaxFactory::makeTokenList({cID});
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, cArgs,
                                   RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      auto swiftArgs = SyntaxFactory::makeTokenList({swiftID});
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen,
                                   swiftArgs, RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      auto blockArgs = SyntaxFactory::makeTokenList({blockID});
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen,
                                   blockArgs,RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", {}, {});
    auto Escaping = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withIdentifier(EscapingID);
    Escaping.print(OS);
    ASSERT_EQ(OS.str().str(), "@escaping");
  }
}

TEST(TypeSyntaxTests, TupleWithAPIs) {
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType()
      .withLeftParen(SyntaxFactory::makeLeftParenToken({}, {}));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType()
      .withLeftParen(SyntaxFactory::makeLeftParenToken({Trivia::spaces(2)},
                                                       {Trivia::spaces(1)}));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "  ( )");
  }
  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType()
      .withRightParen(
        SyntaxFactory::makeRightParenToken({Trivia::spaces(4)},
                                           {Trivia::newlines(1)}));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "(    )\n");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType()
      .withRightParen(SyntaxFactory::makeRightParenToken({}, {}));
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Comma = SyntaxFactory::makeCommaToken({}, {});
    auto Foo = SyntaxFactory::makeTypeIdentifier("Foo", {},
                                                 { Trivia::spaces(2) });
    auto Void = SyntaxFactory::makeVoidTupleType()
      .withElements(SyntaxFactory::makeTupleTypeElementList({
        SyntaxFactory::makeTupleTypeElement(Foo, Comma),
        SyntaxFactory::makeTupleTypeElement(Foo)
      }));
    Void.print(OS);
    ASSERT_EQ(OS.str().str(), "(Foo  ,Foo  )");
  }
}

TEST(TypeSyntaxTests, TupleBuilderAPIs) {
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder;
    Builder.useLeftParen(SyntaxFactory::makeLeftParenToken({}, {}));
    auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
    auto IntId = SyntaxFactory::makeIdentifier("Int", {}, {});
    auto IntType = SyntaxFactory::makeTypeIdentifier(IntId, None, None, None);
    auto Int = SyntaxFactory::makeTupleTypeElement(IntType);
    auto IntWithComma = SyntaxFactory::makeTupleTypeElement(IntType, Comma);
    auto StringId = SyntaxFactory::makeIdentifier("String", {}, {});
    auto StringType = SyntaxFactory::makeTypeIdentifier(StringId, None,
                                                        None, None);
    auto String = SyntaxFactory::makeTupleTypeElement(StringType, Comma);
    Builder.addTupleTypeElement(IntWithComma);
    Builder.addTupleTypeElement(String);
    Builder.addTupleTypeElement(Int);
    Builder.useRightParen(SyntaxFactory::makeRightParenToken({}, {}));

    auto TupleType = Builder.build();

    TupleType.print(OS);
    ASSERT_EQ(OS.str(), "(Int, String, Int)");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };

    TupleTypeSyntaxBuilder Builder;
    Builder.useLeftParen(SyntaxFactory::makeLeftParenToken({}, {}));
    auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
    auto Colon = SyntaxFactory::makeColonToken({}, { Trivia::spaces(1) });
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto xLabel = SyntaxFactory::makeIdentifier("x", {} , {});
    auto xTypeElt = SyntaxFactory::makeTupleTypeElement(xLabel, Colon,
                                                        Int, Comma);
    auto yLabel = SyntaxFactory::makeIdentifier("y", {} , {});
    auto inout = SyntaxFactory::makeInoutKeyword({}, { Trivia::spaces(1) });
    auto yTypeAnnotation = SyntaxFactory::makeTypeAnnotation({}, inout, Int);
    auto yTypeElt = SyntaxFactory::makeTupleTypeElement(yLabel, Colon,
                                                        yTypeAnnotation,
                                                        None);
    Builder.addTupleTypeElement(xTypeElt);
    Builder.addTupleTypeElement(yTypeElt);
    Builder.useRightParen(SyntaxFactory::makeRightParenToken({}, {}));

    auto TupleType = Builder.build();
    TupleType.print(OS);

    ASSERT_EQ(OS.str(), "(x: Int, y: inout Int)");
  }
}

TEST(TypeSyntaxTests, TupleMakeAPIs) {
  {
    SmallString<2> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Void = SyntaxFactory::makeVoidTupleType();
    Void.print(OS);
    ASSERT_EQ(OS.str(), "()");
  }

  {
    SmallString<10> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Bool = SyntaxFactory::makeTypeIdentifier("Bool", {}, {});
    auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
    auto TupleType = SyntaxFactory::makeTupleType(
                        SyntaxFactory::makeLeftParenToken({}, {}),
                        SyntaxFactory::makeTupleTypeElementList({
                          SyntaxFactory::makeTupleTypeElement(Int, Comma),
                          SyntaxFactory::makeTupleTypeElement(Bool, Comma),
                          SyntaxFactory::makeTupleTypeElement(Int, Comma),
                          SyntaxFactory::makeTupleTypeElement(Bool, Comma),
                          SyntaxFactory::makeTupleTypeElement(Int, None)
                        }),
                        SyntaxFactory::makeRightParenToken({}, {}));
    TupleType.print(OS);
    ASSERT_EQ(OS.str().str(),
              "(Int, Bool, Int, Bool, Int)");
  }
}

TEST(TypeSyntaxTests, CreateCannedTypes) {
  {
    SmallString<3> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Any = SyntaxFactory::makeAnyTypeIdentifier();
    Any.print(OS);
    ASSERT_EQ(OS.str(), "Any");
  }

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto Self = SyntaxFactory::makeSelfTypeIdentifier();
    Self.print(OS);
    ASSERT_EQ(OS.str(), "Self");
  }
}

TEST(TypeSyntaxTests, OptionalTypeMakeAPIs) {
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Question = SyntaxFactory::makePostfixQuestionMarkToken({}, {});
    auto OptionalInt = SyntaxFactory::makeOptionalType(Int, Question);
    OptionalInt.print(OS);
    ASSERT_EQ(OS.str(), "Int?");
  }
}

TEST(TypeSyntaxTests, OptionalTypeWithAPIs) {
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType = SyntaxFactory::makeTypeIdentifier("String",
                                                        Trivia::spaces(1), {});
    SyntaxFactory::makeBlankOptionalType()
      .withValueType(StringType)
      .withQuestionMark(SyntaxFactory::makePostfixQuestionMarkToken({}, {}))
      .print(OS);
    ASSERT_EQ(OS.str(), " String?");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeMakeAPIs) {
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Bang = SyntaxFactory::makeExclamationMarkToken({}, {});
    auto IntBang =
      SyntaxFactory::makeImplicitlyUnwrappedOptionalType(Int, Bang);
    IntBang.print(OS);
    ASSERT_EQ(OS.str(), "Int!");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeWithAPIs) {
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType = SyntaxFactory::makeTypeIdentifier("String",
                                                          { Trivia::spaces(1) },
                                                          {});
    SyntaxFactory::makeBlankImplicitlyUnwrappedOptionalType()
      .withValueType(StringType)
      .withExclamationMark(SyntaxFactory::makeExclamationMarkToken({}, {}))
      .print(OS);
    ASSERT_EQ(OS.str(), " String!");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeMakeAPIs) {
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("T", {}, {});
    auto Dot = SyntaxFactory::makePeriodToken({}, {});
    auto Type = SyntaxFactory::makeTypeToken({}, {});
    SyntaxFactory::makeMetatypeType(Int, Dot, Type)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeWithAPIs) {
  auto Int = SyntaxFactory::makeTypeIdentifier("T", {}, {});
  auto Dot = SyntaxFactory::makePeriodToken({}, {});
  auto Type = SyntaxFactory::makeTypeToken({}, {});
  auto Protocol = SyntaxFactory::makeProtocolToken({}, {});

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType()
      .withBaseType(Int)
      .withPeriod(Dot)
      .withTypeOrProtocol(Type)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType()
      .withBaseType(Int)
      .withPeriod(Dot)
      .withTypeOrProtocol(Protocol)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Protocol");
  }

#ifndef NDEBUG
  ASSERT_DEATH({
    SyntaxFactory::makeBlankMetatypeType()
    .withBaseType(Int)
    .withPeriod(Dot)
    .withTypeOrProtocol(SyntaxFactory::makeIdentifier("WRONG", {}, {}));
  }, "");
#endif
}

TEST(TypeSyntaxTests, ArrayTypeWithAPIs) {
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken({}, {});
    auto RightSquare = SyntaxFactory::makeRightSquareBracketToken({}, {});
    auto Double = SyntaxFactory::makeTypeIdentifier("Double", {}, {});
    SyntaxFactory::makeBlankArrayType()
      .withLeftSquareBracket(LeftSquare)
      .withElementType(Double)
      .withRightSquareBracket(RightSquare)
      .print(OS);
    ASSERT_EQ(OS.str(), "[Double]");
  }
}

TEST(TypeSyntaxTests, ArrayTypeMakeAPIs) {
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken({}, {});
    auto RightSquare = SyntaxFactory::makeRightSquareBracketToken({}, {});
    auto Void = SyntaxFactory::makeVoidTupleType();
    SyntaxFactory::makeArrayType(LeftSquare, Void, RightSquare)
      .print(OS);
    ASSERT_EQ(OS.str(), "[()]");
  }
}

TEST(TypeSyntaxTests, DictionaryTypeWithAPIs) {
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken({}, {});
    auto RightSquare = SyntaxFactory::makeRightSquareBracketToken({}, {});
    auto Key = SyntaxFactory::makeTypeIdentifier("String", {},
                                                 { Trivia::spaces(1) });
    auto Value = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, { Trivia::spaces(1) });
    SyntaxFactory::makeBlankDictionaryType()
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
  {
    SmallString<16> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto LeftSquare = SyntaxFactory::makeLeftSquareBracketToken({}, {});
    auto RightSquare = SyntaxFactory::makeRightSquareBracketToken({}, {});
    auto Key = SyntaxFactory::makeTypeIdentifier("String", {},
                                                   { Trivia::spaces(1) });
    auto Value = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, { Trivia::spaces(1) });
    SyntaxFactory::makeDictionaryType(LeftSquare,
                                        Key, Colon, Value,
                                        RightSquare).print(OS);
    ASSERT_EQ(OS.str(), "[String : Int]");
  }
}

TEST(TypeSyntaxTests, FunctionTypeMakeAPIs) {
  auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
  auto Colon = SyntaxFactory::makeColonToken({}, { Trivia::spaces(1) });
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RightParen = SyntaxFactory::makeRightParenToken({},
                                                       {Trivia::spaces(1)});
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  auto IntAnnotation = SyntaxFactory::makeTypeAnnotation({}, None, Int);
  auto IntArg = SyntaxFactory::makeBlankFunctionTypeArgument()
    .withTypeAnnotation(IntAnnotation);
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrowToken({}, Trivia::spaces(1));

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto xArg = SyntaxFactory::makeBlankFunctionTypeArgument()
      .withExternalName(x)
      .withColon(Colon)
      .withTypeAnnotation(IntAnnotation)
      .withTrailingComma(Comma);
    auto yArg = SyntaxFactory::makeBlankFunctionTypeArgument()
      .withExternalName(y)
      .withColon(Colon)
      .withTypeAnnotation(IntAnnotation);

    auto Attrs = SyntaxFactory::makeBlankAttributeList();
    auto TypeList = SyntaxFactory::makeFunctionTypeArgumentList({
      xArg, yArg
    });
    SyntaxFactory::makeFunctionType(Attrs,
                                    LeftParen,
                                    TypeList,
                                    RightParen,
                                    Throws,
                                    Arrow,
                                    Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Attrs = SyntaxFactory::makeBlankAttributeList();
    auto TypeList = SyntaxFactory::makeFunctionTypeArgumentList({
      IntArg.withTrailingComma(Comma),
      IntArg
    });
    SyntaxFactory::makeFunctionType(Attrs,
                                    LeftParen,
                                    TypeList,
                                    RightParen,
                                    Rethrows,
                                    Arrow,
                                    Int).print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Attrs = SyntaxFactory::makeBlankAttributeList();
    auto TypeList = SyntaxFactory::makeBlankFunctionTypeArgumentList();
    auto Void = SyntaxFactory::makeVoidTupleType();
    SyntaxFactory::makeFunctionType(Attrs,
                                    LeftParen,
                                    TypeList,
                                    RightParen,
                                    TokenSyntax::missingToken(tok::kw_throws,
                                                              "throws"),
                                    Arrow,
                                    Void).print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }

}

TEST(TypeSyntaxTests, FunctionTypeWithAPIs) {
  auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RightParen = SyntaxFactory::makeRightParenToken({}, Trivia::spaces(1));
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  auto IntAnnotation = SyntaxFactory::makeTypeAnnotation({}, None, Int);
  auto IntArg = SyntaxFactory::makeFunctionTypeArgument(None, None, None,
                                                        IntAnnotation, None);
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrowToken({}, { Trivia::spaces(1) });

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
    auto intAnnotation = SyntaxFactory::makeTypeAnnotation({}, None, Int);
    auto xArg = SyntaxFactory::makeFunctionTypeArgument(x, None, Colon,
                                                        intAnnotation, Comma);
    auto yArg = SyntaxFactory::makeFunctionTypeArgument(y, None, Colon,
                                                        intAnnotation, None);

    SyntaxFactory::makeBlankFunctionType()
      .withLeftParen(LeftParen)
      .addFunctionTypeArgument(xArg)
      .addFunctionTypeArgument(yArg)
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
    SyntaxFactory::makeBlankFunctionType()
      .withLeftParen(LeftParen)
      .withRightParen(RightParen)
      .addFunctionTypeArgument(IntArg.withTrailingComma(Comma))
      .addFunctionTypeArgument(IntArg)
      .withThrowsOrRethrowsKeyword(Rethrows)
      .withArrow(Arrow)
      .withReturnType(Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto Void = SyntaxFactory::makeVoidTupleType();
    SyntaxFactory::makeBlankFunctionType()
      .withLeftParen(LeftParen)
      .withRightParen(RightParen)
      .withArrow(Arrow)
      .withReturnType(Void)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}

TEST(TypeSyntaxTests, FunctionTypeBuilderAPIs) {
  auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  auto IntAnnotation =
    SyntaxFactory::makeTypeAnnotation({}, None, Int);
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RightParen = SyntaxFactory::makeRightParenToken({},
                                                         {Trivia::spaces(1)});
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrowToken({}, { Trivia::spaces(1) });

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
    auto xArg = SyntaxFactory::makeFunctionTypeArgument(x, None, Colon,
                                                        IntAnnotation, Comma);
    auto yArg = SyntaxFactory::makeFunctionTypeArgument(y, None, Colon,
                                                        IntAnnotation, None);

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addFunctionTypeArgument(xArg)
      .addFunctionTypeArgument(yArg)
      .useThrowsOrRethrowsKeyword(Throws)
      .useArrow(Arrow)
      .useReturnType(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto IntArg = SyntaxFactory::makeFunctionTypeArgument(None, None,
                                                          None, IntAnnotation,
                                                          None);
    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addFunctionTypeArgument(IntArg.withTrailingComma(Comma))
      .addFunctionTypeArgument(IntArg)
      .useThrowsOrRethrowsKeyword(Rethrows)
      .useArrow(Arrow)
      .useReturnType(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto Void = SyntaxFactory::makeVoidTupleType();

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .useArrow(Arrow)
      .useReturnType(Void);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}
