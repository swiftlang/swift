#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TypeSyntax.h"
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
    auto Autoclosure = SyntaxFactory::makeBlankTypeAttribute()
      .withAtSignToken(At)
      .withIdentifier(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = SyntaxFactory::makeIdentifier("convention", {}, {});
    auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
    auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

    auto Convention = SyntaxFactory::makeBlankTypeAttribute()
      .withAtSignToken(At)
      .withIdentifier(conventionID)
      .withLeftParenToken(LeftParen)
      .withRightParenToken(RightParen);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", {}, {});
      auto cArgs = SyntaxFactory::makeBalancedTokens({cID});
      Convention.withBalancedTokens(cArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      auto swiftArgs = SyntaxFactory::makeBalancedTokens({swiftID});
      Convention.withBalancedTokens(swiftArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      auto blockArgs = SyntaxFactory::makeBalancedTokens({blockID});
      Convention.withBalancedTokens(blockArgs).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", {}, {});
    auto Escaping = SyntaxFactory::makeBlankTypeAttribute()
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
    auto Autoclosure = SyntaxFactory::makeBlankTypeAttribute()
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
      auto cArgs = SyntaxFactory::makeBalancedTokens({cID});
      SyntaxFactory::makeTypeAttribute(At, conventionID,
                                       LeftParen, cArgs, RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      auto swiftArgs = SyntaxFactory::makeBalancedTokens({swiftID});
      SyntaxFactory::makeTypeAttribute(At, conventionID,
                                       LeftParen, swiftArgs, RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      auto blockArgs = SyntaxFactory::makeBalancedTokens({blockID});
      SyntaxFactory::makeTypeAttribute(At, conventionID,
                                       LeftParen, blockArgs, RightParen)
        .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", {}, {});
    auto Escaping = SyntaxFactory::makeBlankTypeAttribute()
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
      .withTypeElementList(SyntaxFactory::makeTupleTypeElementList({
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
    auto IntId = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Int = SyntaxFactory::makeTupleTypeElement(IntId);
    auto IntWithComma = SyntaxFactory::makeTupleTypeElement(IntId, Comma);
    auto StringId = SyntaxFactory::makeTypeIdentifier("String", {}, {});
    auto String = SyntaxFactory::makeTupleTypeElement(StringId, Comma);
    Builder.addElementTypeSyntax(IntWithComma);
    Builder.addElementTypeSyntax(String);
    Builder.addElementTypeSyntax(Int);
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
    auto yTypeElt = SyntaxFactory::makeTupleTypeElement(yLabel, Colon, Int)
      .withInoutToken(
        SyntaxFactory::makeInoutKeyword({}, { Trivia::spaces(1) }));
    Builder.addElementTypeSyntax(xTypeElt);
    Builder.addElementTypeSyntax(yTypeElt);
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
    auto OptionalInt = SyntaxFactory::makeOptionalType(Int, {});
    OptionalInt.print(OS);
    ASSERT_EQ(OS.str(), "Int?");
  }
}

TEST(TypeSyntaxTests, OptionalTypeWithAPIs) {
  {
    SmallString<8> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto StringType = SyntaxFactory::makeTypeIdentifier("String",
                                                          { Trivia::spaces(1) },
                                                          {});
    SyntaxFactory::makeBlankOptionalType()
      .withBaseTypeSyntax(StringType)
      .withQuestionToken(SyntaxFactory::makeQuestionPostfixToken({}))
      .print(OS);
    ASSERT_EQ(OS.str(), " String?");
  }
}

TEST(TypeSyntaxTests, ImplicitlyUnwrappedOptionalTypeMakeAPIs) {
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto IntBang = SyntaxFactory::makeImplicitlyUnwrappedOptionalType(Int, {});
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
      .withBaseTypeSyntax(StringType)
      .withExclaimToken(SyntaxFactory::makeExclaimPostfixToken({}))
      .print(OS);
    ASSERT_EQ(OS.str(), " String!");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeMakeAPIs) {
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto Int = SyntaxFactory::makeTypeIdentifier("T", {}, {});
    auto Dot = SyntaxFactory::makeDotToken({}, {});
    auto Type = SyntaxFactory::makeTypeToken({}, {});
    SyntaxFactory::makeMetatypeType(Int, Dot, Type)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
}

TEST(TypeSyntaxTests, MetatypeTypeWithAPIs) {
  auto Int = SyntaxFactory::makeTypeIdentifier("T", {}, {});
  auto Dot = SyntaxFactory::makeDotToken({}, {});
  auto Type = SyntaxFactory::makeTypeToken({}, {});
  auto Protocol = SyntaxFactory::makeProtocolToken({}, {});

  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType()
      .withBaseTypeSyntax(Int)
      .withDotToken(Dot)
      .withTypeToken(Type)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Type");
  }
  {
    SmallString<4> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankMetatypeType()
      .withBaseTypeSyntax(Int)
      .withDotToken(Dot)
      .withTypeToken(Protocol)
      .print(OS);
    ASSERT_EQ(OS.str(), "T.Protocol");
  }

#ifndef NDEBUG
  ASSERT_DEATH({
    SyntaxFactory::makeBlankMetatypeType()
    .withBaseTypeSyntax(Int)
    .withDotToken(Dot)
    .withTypeToken(SyntaxFactory::makeIdentifier("WRONG", {}, {}));
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
      .withLeftSquareBracketToken(LeftSquare)
      .withType(Double)
      .withRightSquareBracketToken(RightSquare)
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
      .withLeftSquareBracketToken(LeftSquare)
      .withKeyTypeSyntax(Key)
      .withColon(Colon)
      .withValueTypeSyntax(Value)
      .withRightSquareBracketToken(RightSquare)
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
  auto IntArg = SyntaxFactory::makeBlankFunctionParameter()
    .withTypeSyntax(Int);
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrow({}, { Trivia::spaces(1) });

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto xArg = SyntaxFactory::makeBlankFunctionParameter()
      .withExternalName(x)
      .withColonToken(Colon)
      .withTypeSyntax(Int)
      .withTrailingComma(Comma);
    auto yArg = SyntaxFactory::makeBlankFunctionParameter()
      .withExternalName(y)
      .withColonToken(Colon)
      .withTypeSyntax(Int);

    auto Attrs = SyntaxFactory::makeBlankTypeAttributes();
    auto TypeList = SyntaxFactory::makeFunctionParameterList({
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
    auto Attrs = SyntaxFactory::makeBlankTypeAttributes();
    auto TypeList = SyntaxFactory::makeFunctionParameterList({
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
    auto Attrs = SyntaxFactory::makeBlankTypeAttributes();
    auto TypeList = SyntaxFactory::makeBlankFunctionParameterList();
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
  auto RightParen = SyntaxFactory::makeRightParenToken({},
                                                         {Trivia::spaces(1)});
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  auto IntArg = SyntaxFactory::makeFunctionTypeArgument(Int);
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrow({}, { Trivia::spaces(1) });

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
    auto xArg = SyntaxFactory::makeFunctionTypeArgument(x, Colon, Int);
    auto yArg = SyntaxFactory::makeFunctionTypeArgument(y, Colon, Int);

    SyntaxFactory::makeBlankFunctionType()
      .withLeftArgumentsParen(LeftParen)
      .addTypeArgument(None, xArg)
      .addTypeArgument(Comma, yArg)
      .withRightArgumentsParen(RightParen)
      .withThrowsKeyword(Throws)
      .withArrow(Arrow)
      .withReturnTypeSyntax(Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");


  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    SyntaxFactory::makeBlankFunctionType()
      .withLeftArgumentsParen(LeftParen)
      .withRightArgumentsParen(RightParen)
      .addTypeArgument(None, IntArg)
      .addTypeArgument(Comma, IntArg)
      .withRethrowsKeyword(Rethrows)
      .withArrow(Arrow)
      .withReturnTypeSyntax(Int)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }
  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto Void = SyntaxFactory::makeVoidTupleType();
    SyntaxFactory::makeBlankFunctionType()
      .withLeftArgumentsParen(LeftParen)
      .withRightArgumentsParen(RightParen)
      .withArrow(Arrow)
      .withReturnTypeSyntax(Void)
      .print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}

TEST(TypeSyntaxTests, FunctionTypeBuilderAPIs) {
  auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
  auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
  auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
  auto RightParen = SyntaxFactory::makeRightParenToken({},
                                                         {Trivia::spaces(1)});
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrow({}, { Trivia::spaces(1) });

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto Colon = SyntaxFactory::makeColonToken({}, Trivia::spaces(1));
    auto xArg = SyntaxFactory::makeFunctionTypeArgument(x, Colon, Int);
    auto yArg = SyntaxFactory::makeFunctionTypeArgument(y, Colon, Int);

    Builder.useLeftArgumentsParen(LeftParen)
      .useRightArgumentsParen(RightParen)
      .addArgumentTypeSyntax(None, xArg)
      .addArgumentTypeSyntax(Comma, yArg)
      .useThrowsKeyword(Throws)
      .useArrow(Arrow)
      .useReturnTypeSyntax(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(x: Int, y: Int) throws -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto IntId = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto IntArg = SyntaxFactory::makeFunctionTypeArgument(IntId);
    Builder.useLeftArgumentsParen(LeftParen)
      .useRightArgumentsParen(RightParen)
      .addArgumentTypeSyntax(None, IntArg)
      .addArgumentTypeSyntax(Comma, IntArg)
      .useRethrowsKeyword(Rethrows)
      .useArrow(Arrow)
      .useReturnTypeSyntax(Int);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "(Int, Int) rethrows -> Int");
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FunctionTypeSyntaxBuilder Builder;
    auto Void = SyntaxFactory::makeVoidTupleType();

    Builder.useLeftArgumentsParen(LeftParen)
      .useRightArgumentsParen(RightParen)
      .useArrow(Arrow)
      .useReturnTypeSyntax(Void);

    Builder.build().print(OS);
    ASSERT_EQ(OS.str().str(), "() -> ()");
  }
}
