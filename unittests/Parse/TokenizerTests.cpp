#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

// The test fixture.
class TokenizerTest : public ::testing::Test {
public:
  LangOptions LangOpts;
  SourceManager SM;

  unsigned makeBuffer(StringRef Source) {
    return SM.addMemBufferCopy(Source);
  }

  static void replaceNewLines(std::string &S) {
    size_t Index = 0;
    while (true) {
      Index = S.find("\n", Index);
      if (Index == std::string::npos) break;
      S.erase(Index, 1);
      S.insert(Index, "\\n");
      Index += 3;
    }
  }

  static StringRef tokToString(swift::tok T) {
    switch (T) {
  #define KEYWORD(X) \
    case swift::tok::kw_##X: return "kw_" #X; break;
  #define PUNCTUATOR(X, Y) \
    case swift::tok::X: return #X; break;
#define POUND(X, Y) \
    case swift::tok::pound_##X: return "pound_" #X; break;
  #include "swift/Syntax/TokenKinds.def"

  #define OTHER(X) \
  case swift::tok::X: return #X; break;
        OTHER(unknown)
        OTHER(eof)
        OTHER(code_complete)
        OTHER(identifier)
        OTHER(oper_binary_unspaced)
        OTHER(oper_binary_spaced)
        OTHER(oper_postfix)
        OTHER(oper_prefix)
        OTHER(dollarident)
        OTHER(integer_literal)
        OTHER(floating_literal)
        OTHER(string_literal)
        OTHER(sil_local_name)
        OTHER(comment)

      default:
        return "??? (" + std::to_string(static_cast<int>(tok())) + ")";
        break;
    }
  }
  
  void assertTokens(std::vector<Token> Ts, StringRef Expected) {
    std::string Actual;
    for (auto C = Ts.begin(), E = Ts.end(); C != E; ++C) {
      Actual += tokToString(C->getKind());
      Actual += ": ";
      
      std::string Txt(C->getRawText());
      replaceNewLines(Txt);
      Actual += Txt;
      
      Actual += "\n";
    }
    EXPECT_EQ(Expected, Actual)
      << "---- Expected: \n" << Expected << "\n" 
      << "---- Actual: \n" << Actual << "\n";
  }
  
  std::vector<Token> parseAndGetSplitTokens(unsigned BufID) {
    swift::ParserUnit PU(SM, BufID, LangOpts, "unknown");

    bool Done = false;
    while (!Done) {
      PU.getParser().parseTopLevel();
      Done = PU.getParser().Tok.is(tok::eof);
    }
    
    return PU.getParser().getSplitTokens();
  }
  
  std::vector<Token> tokenize(unsigned BufID, const std::vector<Token> &SplitTokens = {}) {
    return swift::tokenize(LangOpts, 
                           SM, 
                           BufID, 
                           /* Offset = */ 0,
                           /* EndOffset = */ 0,
                           /* KeepComments = */ true,
                           /* TokenizeInterpolatedString = */ true,
                           SplitTokens);
  }
};

TEST_F(TokenizerTest, ProperlySplitTokens) {
  auto BufID = makeBuffer(
    "infix operator ⊕ { associativity left precedence 100 }\n"
    "func ⊕<T>(t1: T, t2: T) {}\n"
  );
  
  // Tokenize w/o fixing split tokens
  auto Tokens = tokenize(BufID);
  assertTokens(Tokens,
    "identifier: infix\n"
    "kw_operator: operator\n"
    "oper_binary_spaced: ⊕\n"
    "l_brace: {\n"
    "identifier: associativity\n"
    "identifier: left\n"
    "identifier: precedence\n"
    "integer_literal: 100\n"
    "r_brace: }\n"
    "kw_func: func\n"
    "oper_prefix: ⊕<\n"
    "identifier: T\n"
    "oper_binary_unspaced: >\n"
    "l_paren: (\n"
    "identifier: t1\n"
    "colon: :\n"
    "identifier: T\n"
    "comma: ,\n"
    "identifier: t2\n"
    "colon: :\n"
    "identifier: T\n"
    "r_paren: )\n"
    "l_brace: {\n"
    "r_brace: }\n"
  );

  // Parse the input and get split tokens info
  auto SplitTokens = parseAndGetSplitTokens(BufID);

  // Tokenize with fixing split tokens
  Tokens = tokenize(BufID, SplitTokens);
  assertTokens(Tokens,
     "identifier: infix\n"
     "kw_operator: operator\n"
     "oper_binary_spaced: ⊕\n"
     "l_brace: {\n"
     "identifier: associativity\n"
     "identifier: left\n"
     "identifier: precedence\n"
     "integer_literal: 100\n"
     "r_brace: }\n"
     "kw_func: func\n"
     "identifier: ⊕\n"
     "oper_binary_unspaced: <\n"
     "identifier: T\n"
     "oper_binary_unspaced: >\n"
     "l_paren: (\n"
     "identifier: t1\n"
     "colon: :\n"
     "identifier: T\n"
     "comma: ,\n"
     "identifier: t2\n"
     "colon: :\n"
     "identifier: T\n"
     "r_paren: )\n"
     "l_brace: {\n"
     "r_brace: }\n"
  );
}

