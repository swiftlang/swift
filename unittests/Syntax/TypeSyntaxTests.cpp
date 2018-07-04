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
      .withAttributeName(AutoclosureID);
    Autoclosure.print(OS);
    ASSERT_EQ(OS.str().str(), "@autoclosure");
  }

  {
    auto conventionID = SyntaxFactory::makeIdentifier("convention", {}, {});
    auto LeftParen = SyntaxFactory::makeLeftParenToken({}, {});
    auto RightParen = SyntaxFactory::makeRightParenToken({}, {});

    auto Convention = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withAttributeName(conventionID)
      .withLeftParen(LeftParen)
      .withRightParen(RightParen);

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto cID = SyntaxFactory::makeIdentifier("c", {}, {});
      Convention.withArgument(cID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      auto swiftArgs = SyntaxFactory::makeTokenList({LeftParen, swiftID, RightParen});
      Convention.withArgument(swiftID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      auto blockArgs = SyntaxFactory::makeTokenList({LeftParen, blockID, RightParen});
      Convention.withArgument(blockID).print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(block)");
    }
  }

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS { Scratch };
    auto EscapingID = SyntaxFactory::makeIdentifier("escaping", {}, {});
    auto Escaping = SyntaxFactory::makeBlankAttribute()
      .withAtSignToken(At)
      .withAttributeName(EscapingID);
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
    .withAttributeName(AutoclosureID);
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
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, cID, RightParen,
                                   llvm::None)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(c)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto swiftID = SyntaxFactory::makeIdentifier("swift", {}, {});
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, swiftID,
                                   RightParen, llvm::None)
          .print(OS);
      ASSERT_EQ(OS.str().str(), "@convention(swift)");
    }

    {
      SmallString<48> Scratch;
      llvm::raw_svector_ostream OS { Scratch };
      auto blockID = SyntaxFactory::makeIdentifier("block", {}, {});
      SyntaxFactory::makeAttribute(At, conventionID, LeftParen, blockID,
                                   RightParen, llvm::None)
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
      .withAttributeName(EscapingID);
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
    auto IntType = SyntaxFactory::makeSimpleTypeIdentifier(IntId, None);
    auto Int = SyntaxFactory::makeTupleTypeElement(IntType);
    auto IntWithComma = SyntaxFactory::makeTupleTypeElement(IntType, Comma);
    auto StringId = SyntaxFactory::makeIdentifier("String", {}, {});
    auto StringType = SyntaxFactory::makeSimpleTypeIdentifier(StringId, None);
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
    auto Int = SyntaxFactory::makeTypeIdentifier("Int", {}, {});
    auto Comma = SyntaxFactory::makeCommaToken({}, { Trivia::spaces(1) });
    auto Colon = SyntaxFactory::makeColonToken({}, { Trivia::spaces(1) });
    auto xLabel = SyntaxFactory::makeIdentifier("x", {} , {});
    auto xTypeElt = SyntaxFactory::makeTupleTypeElement(xLabel, Colon,
                                                        Int, Comma);
    auto inout = SyntaxFactory::makeInoutKeyword({}, { Trivia::spaces(1) });
    auto yLabel = SyntaxFactory::makeIdentifier("y", {} , {});
    auto yTypeElt = SyntaxFactory::makeTupleTypeElement(yLabel, Colon,
                                                        Int)
      .withInOut(inout);
    Builder.addTupleTypeElement(xTypeElt);
    Builder.addTupleTypeElement(yTypeElt);
    Builder.useRightParen(SyntaxFactory::makeRightParenToken({}, {}));

    auto TupleType = Builder.build();
    TupleType.print(OS);

    ASSERT_EQ(OS.str(), "(x: Int, inout y: Int)");
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
      .withWrappedType(StringType)
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
      .withWrappedType(StringType)
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
  auto IntArg = SyntaxFactory::makeBlankTupleTypeElement()
    .withType(Int);
  auto Throws = SyntaxFactory::makeThrowsKeyword({}, { Trivia::spaces(1) });
  auto Rethrows = SyntaxFactory::makeRethrowsKeyword({},
                                                       { Trivia::spaces(1) });
  auto Arrow = SyntaxFactory::makeArrowToken({}, Trivia::spaces(1));

  {
    SmallString<48> Scratch;
    llvm::raw_svector_ostream OS(Scratch);

    auto x = SyntaxFactory::makeIdentifier("x", {}, {});
    auto y = SyntaxFactory::makeIdentifier("y", {}, {});
    auto xArg = SyntaxFactory::makeBlankTupleTypeElement()
      .withName(x)
      .withColon(Colon)
      .withType(Int)
      .withTrailingComma(Comma);
    auto yArg = SyntaxFactory::makeBlankTupleTypeElement()
      .withName(y)
      .withColon(Colon)
      .withType(Int);

    auto TypeList = SyntaxFactory::makeTupleTypeElementList({
      xArg, yArg
    });
    SyntaxFactory::makeFunctionType(LeftParen,
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
    auto TypeList = SyntaxFactory::makeTupleTypeElementList({
      IntArg.withTrailingComma(Comma),
      IntArg
    });
    SyntaxFactory::makeFunctionType(LeftParen,
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
    auto TypeList = SyntaxFactory::makeBlankTupleTypeElementList();
    auto Void = SyntaxFactory::makeVoidTupleType();
    SyntaxFactory::makeFunctionType(LeftParen,
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
  auto IntArg = SyntaxFactory::makeTupleTypeElement(None, None, None, None,
                                                    Int, None, None, None);
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
    auto xArg = SyntaxFactory::makeTupleTypeElement(None, x, None, Colon,
                                                    Int, None, None, Comma);
    auto yArg = SyntaxFactory::makeTupleTypeElement(None, y, None, Colon,
                                                    Int, None, None, None);

    SyntaxFactory::makeBlankFunctionType()
      .withLeftParen(LeftParen)
      .addTupleTypeElement(xArg)
      .addTupleTypeElement(yArg)
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
      .addTupleTypeElement(IntArg.withTrailingComma(Comma))
      .addTupleTypeElement(IntArg)
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
    auto xArg = SyntaxFactory::makeTupleTypeElement(None, x, None, Colon,
                                                    Int, None, None, Comma);
    auto yArg = SyntaxFactory::makeTupleTypeElement(None, y, None, Colon,
                                                    Int, None, None, None);

    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addTupleTypeElement(xArg)
      .addTupleTypeElement(yArg)
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
    auto IntArg = SyntaxFactory::makeTupleTypeElement(None, None, None, None,
                                                      Int, None, None, None);
    Builder.useLeftParen(LeftParen)
      .useRightParen(RightParen)
      .addTupleTypeElement(IntArg.withTrailingComma(Comma))
      .addTupleTypeElement(IntArg)
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
