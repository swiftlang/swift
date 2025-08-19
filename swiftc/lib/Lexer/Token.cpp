#include "swiftc/Lexer/Token.h"
#include <llvm/ADT/StringSwitch.h>

using namespace swiftc;

bool Token::isKeyword() const {
  switch (Kind) {
  case TokenKind::Let:
  case TokenKind::Var:
  case TokenKind::Func:
  case TokenKind::Class:
  case TokenKind::Struct:
  case TokenKind::Enum:
  case TokenKind::If:
  case TokenKind::Else:
  case TokenKind::While:
  case TokenKind::For:
  case TokenKind::Return:
  case TokenKind::Import:
  case TokenKind::Switch:
  case TokenKind::Case:
  case TokenKind::Break:
  case TokenKind::Continue:
  case TokenKind::In:
  case TokenKind::True:
  case TokenKind::False:
  case TokenKind::Nil:
  case TokenKind::Init:
  case TokenKind::Infix:
  case TokenKind::Prefix:
  case TokenKind::Postfix:
  case TokenKind::Operator:
  case TokenKind::Precedencegroup:
  case TokenKind::Associatedtype:
  case TokenKind::Typealias:
  case TokenKind::Throw:
  case TokenKind::Try:
  case TokenKind::Catch:
  case TokenKind::Guard:
  case TokenKind::Do:
  case TokenKind::Repeat:
  case TokenKind::Default:
  case TokenKind::Where:
  case TokenKind::As:
  case TokenKind::Is:
  case TokenKind::Self_:
  case TokenKind::Super:
  case TokenKind::Some:
  case TokenKind::Any:
  case TokenKind::Mutating:
  case TokenKind::Nonmutating:
  case TokenKind::Override:
  case TokenKind::Final:
  case TokenKind::Lazy:
  case TokenKind::Weak:
  case TokenKind::Unowned:
  case TokenKind::Required:
  case TokenKind::Convenience:
  case TokenKind::Dynamic:
  case TokenKind::Optional:
  case TokenKind::Public:
  case TokenKind::Private:
  case TokenKind::Internal:
  case TokenKind::Fileprivate:
  case TokenKind::Open:
  case TokenKind::Static:
  case TokenKind::Get:
  case TokenKind::Set:
  case TokenKind::Willset:
  case TokenKind::Didset:
  case TokenKind::Deinit:
  case TokenKind::Subscript:
  case TokenKind::Extension:
  case TokenKind::Protocol:
  case TokenKind::Throws:
  case TokenKind::Rethrows:
  case TokenKind::HigherThan:
  case TokenKind::LowerThan:
  case TokenKind::Associativity:
  case TokenKind::Left:
  case TokenKind::Right:
  case TokenKind::None:
  case TokenKind::Inout:
    return true;
  default:
    return false;
  }
}

bool Token::isOperator() const {
  switch (Kind) {
  case TokenKind::Plus:
  case TokenKind::Minus:
  case TokenKind::Star:
  case TokenKind::Slash:
  case TokenKind::Equal:
  case TokenKind::EqualEqual:
  case TokenKind::Less:
  case TokenKind::Greater:
  case TokenKind::Arrow:
  case TokenKind::FatArrow:
  case TokenKind::Dot:
  case TokenKind::DotDotDot:
  case TokenKind::DotDotLess:
  case TokenKind::ExclaimEqual:
  case TokenKind::LessEqual:
  case TokenKind::GreaterEqual:
  case TokenKind::AmpAmp:
  case TokenKind::PipePipe:
  case TokenKind::Exclaim:
  case TokenKind::Question:
  case TokenKind::QuestionQuestion:
  case TokenKind::Amp:
  case TokenKind::Pipe:
  case TokenKind::Caret:
  case TokenKind::Tilde:
    return true;
  default:
    return false;
  }
}

bool Token::isLiteral() const {
  switch (Kind) {
  case TokenKind::IntegerLiteral:
  case TokenKind::FloatingPointLiteral:
  case TokenKind::StringLiteral:
    return true;
  default:
    return false;
  }
}

bool Token::isAtStartOfLine() const {
  return false;
}

StringRef swiftc::getTokenKindName(TokenKind kind) {
  switch (kind) {
  case TokenKind::Eof: return "eof";
  case TokenKind::Unknown: return "unknown";
  case TokenKind::Identifier: return "identifier";
  case TokenKind::IntegerLiteral: return "integer_literal";
  case TokenKind::FloatingPointLiteral: return "floating_point_literal";
  case TokenKind::StringLiteral: return "string_literal";
  case TokenKind::Let: return "let";
  case TokenKind::Var: return "var";
  case TokenKind::Func: return "func";
  case TokenKind::Class: return "class";
  case TokenKind::Struct: return "struct";
  case TokenKind::Enum: return "enum";
  case TokenKind::If: return "if";
  case TokenKind::Else: return "else";
  case TokenKind::While: return "while";
  case TokenKind::For: return "for";
  case TokenKind::Return: return "return";
  case TokenKind::Import: return "import";
  case TokenKind::Switch: return "switch";
  case TokenKind::Case: return "case";
  case TokenKind::Break: return "break";
  case TokenKind::Continue: return "continue";
  case TokenKind::In: return "in";
  case TokenKind::True: return "true";
  case TokenKind::False: return "false";
  case TokenKind::Nil: return "nil";
  case TokenKind::Init: return "init";
  case TokenKind::Infix: return "infix";
  case TokenKind::Prefix: return "prefix";
  case TokenKind::Postfix: return "postfix";
  case TokenKind::Operator: return "operator";
  case TokenKind::Precedencegroup: return "precedencegroup";
  case TokenKind::Associatedtype: return "associatedtype";
  case TokenKind::Typealias: return "typealias";
  case TokenKind::Throw: return "throw";
  case TokenKind::Try: return "try";
  case TokenKind::Catch: return "catch";
  case TokenKind::Guard: return "guard";
  case TokenKind::Do: return "do";
  case TokenKind::Repeat: return "repeat";
  case TokenKind::Default: return "default";
  case TokenKind::Where: return "where";
  case TokenKind::As: return "as";
  case TokenKind::Is: return "is";
  case TokenKind::Self_: return "self";
  case TokenKind::Super: return "super";
  case TokenKind::Some: return "some";
  case TokenKind::Any: return "any";
  case TokenKind::Mutating: return "mutating";
  case TokenKind::Nonmutating: return "nonmutating";
  case TokenKind::Override: return "override";
  case TokenKind::Final: return "final";
  case TokenKind::Lazy: return "lazy";
  case TokenKind::Weak: return "weak";
  case TokenKind::Unowned: return "unowned";
  case TokenKind::Required: return "required";
  case TokenKind::Convenience: return "convenience";
  case TokenKind::Dynamic: return "dynamic";
  case TokenKind::Optional: return "optional";
  case TokenKind::Public: return "public";
  case TokenKind::Private: return "private";
  case TokenKind::Internal: return "internal";
  case TokenKind::Fileprivate: return "fileprivate";
  case TokenKind::Open: return "open";
  case TokenKind::Static: return "static";
  case TokenKind::Get: return "get";
  case TokenKind::Set: return "set";
  case TokenKind::Willset: return "willSet";
  case TokenKind::Didset: return "didSet";
  case TokenKind::Deinit: return "deinit";
  case TokenKind::Subscript: return "subscript";
  case TokenKind::Extension: return "extension";
  case TokenKind::Protocol: return "protocol";
  case TokenKind::Throws: return "throws";
  case TokenKind::Rethrows: return "rethrows";
  case TokenKind::HigherThan: return "higherThan";
  case TokenKind::LowerThan: return "lowerThan";
  case TokenKind::Associativity: return "associativity";
  case TokenKind::Left: return "left";
  case TokenKind::Right: return "right";
  case TokenKind::None: return "none";
  case TokenKind::Inout: return "inout";
  case TokenKind::Plus: return "+";
  case TokenKind::Minus: return "-";
  case TokenKind::Star: return "*";
  case TokenKind::Slash: return "/";
  case TokenKind::Equal: return "=";
  case TokenKind::EqualEqual: return "==";
  case TokenKind::Less: return "<";
  case TokenKind::Greater: return ">";
  case TokenKind::ExclaimEqual: return "!=";
  case TokenKind::LessEqual: return "<=";
  case TokenKind::GreaterEqual: return ">=";
  case TokenKind::AmpAmp: return "&&";
  case TokenKind::PipePipe: return "||";
  case TokenKind::Exclaim: return "!";
  case TokenKind::Question: return "?";
  case TokenKind::QuestionQuestion: return "??";
  case TokenKind::Amp: return "&";
  case TokenKind::Pipe: return "|";
  case TokenKind::Caret: return "^";
  case TokenKind::Tilde: return "~";
  case TokenKind::Arrow: return "->";
  case TokenKind::FatArrow: return "=>";
  case TokenKind::At: return "@";
  case TokenKind::Hash: return "#";
  case TokenKind::Dollar: return "$";
  case TokenKind::Backtick: return "`";
  case TokenKind::Backslash: return "\\";
  case TokenKind::Dot: return ".";
  case TokenKind::DotDotDot: return "...";
  case TokenKind::DotDotLess: return "..<";
  case TokenKind::LeftParen: return "(";
  case TokenKind::RightParen: return ")";
  case TokenKind::LeftBrace: return "{";
  case TokenKind::RightBrace: return "}";
  case TokenKind::Comma: return ",";
  case TokenKind::Colon: return ":";
  case TokenKind::LeftBraceBrace: return "{{";
  case TokenKind::RightBraceBrace: return "}}";
  default: return "unknown";
  }
}

TokenKind swiftc::getKeywordKind(StringRef text) {
  return llvm::StringSwitch<TokenKind>(text)
    .Case("let", TokenKind::Let)
    .Case("var", TokenKind::Var)
    .Case("func", TokenKind::Func)
    .Case("class", TokenKind::Class)
    .Case("struct", TokenKind::Struct)
    .Case("enum", TokenKind::Enum)
    .Case("if", TokenKind::If)
    .Case("else", TokenKind::Else)
    .Case("while", TokenKind::While)
    .Case("for", TokenKind::For)
    .Case("return", TokenKind::Return)
    .Case("import", TokenKind::Import)
    .Case("switch", TokenKind::Switch)
    .Case("case", TokenKind::Case)
    .Case("break", TokenKind::Break)
    .Case("continue", TokenKind::Continue)
    .Case("in", TokenKind::In)
    .Case("true", TokenKind::True)
    .Case("false", TokenKind::False)
    .Case("nil", TokenKind::Nil)
    .Case("init", TokenKind::Init)
    .Case("infix", TokenKind::Infix)
    .Case("prefix", TokenKind::Prefix)
    .Case("postfix", TokenKind::Postfix)
    .Case("operator", TokenKind::Operator)
    .Case("precedencegroup", TokenKind::Precedencegroup)
    .Case("associatedtype", TokenKind::Associatedtype)
    .Case("typealias", TokenKind::Typealias)
    .Case("throws", TokenKind::Throw)
    .Case("rethrows", TokenKind::Throw)
    .Case("try", TokenKind::Try)
    .Case("catch", TokenKind::Catch)
    .Case("guard", TokenKind::Guard)
    .Case("do", TokenKind::Do)
    .Case("repeat", TokenKind::Repeat)
    .Case("default", TokenKind::Default)
    .Case("where", TokenKind::Where)
    .Case("as", TokenKind::As)
    .Case("is", TokenKind::Is)
    .Case("self", TokenKind::Self_)
    .Case("super", TokenKind::Super)
    .Case("some", TokenKind::Some)
    .Case("any", TokenKind::Any)
    .Case("mutating", TokenKind::Mutating)
    .Case("nonmutating", TokenKind::Nonmutating)
    .Case("override", TokenKind::Override)
    .Case("final", TokenKind::Final)
    .Case("lazy", TokenKind::Lazy)
    .Case("weak", TokenKind::Weak)
    .Case("unowned", TokenKind::Unowned)
    .Case("required", TokenKind::Required)
    .Case("convenience", TokenKind::Convenience)
    .Case("dynamic", TokenKind::Dynamic)
    .Case("optional", TokenKind::Optional)
    .Case("public", TokenKind::Public)
    .Case("private", TokenKind::Private)
    .Case("internal", TokenKind::Internal)
    .Case("fileprivate", TokenKind::Fileprivate)
    .Case("open", TokenKind::Open)
    .Case("static", TokenKind::Static)
    .Case("get", TokenKind::Get)
    .Case("set", TokenKind::Set)
    .Case("willSet", TokenKind::Willset)
    .Case("didSet", TokenKind::Didset)
    .Case("deinit", TokenKind::Deinit)
    .Case("subscript", TokenKind::Subscript)
    .Case("extension", TokenKind::Extension)
    .Case("protocol", TokenKind::Protocol)
    .Case("throws", TokenKind::Throws)
    .Case("rethrows", TokenKind::Rethrows)
    .Case("higherThan", TokenKind::HigherThan)
    .Case("lowerThan", TokenKind::LowerThan)
    .Case("associativity", TokenKind::Associativity)
    .Case("left", TokenKind::Left)
    .Case("right", TokenKind::Right)
    .Case("none", TokenKind::None)
    .Case("inout", TokenKind::Inout)
    .Default(TokenKind::Identifier);
}