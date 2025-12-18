// RUN: %target-typecheck-verify-swift -solver-disable-splitter

// FIXME: This uses too much memory with the splitter.

// https://github.com/swiftlang/swift/issues/43274

public enum Symbol: String {
   case ThisKeyword = "this"
   case NewKeyword = "new"
   case SubtractionOperator = "-"
   case MultiplicationOperator = "*"
   case Period = "."
   case DivisionOperator = "/"
   case ModulusOperator = "%"
   case AdditionOperator = "+"
   case GreaterThanOperator = ">"
   case LessThanOperator = "<"
   case GreaterThanEqualOperator = ">="
   case LessThanEqualsOperator = "<="
   case InstanceofKeyword = "instanceof"
   case EqualsEqualsOperator = "=="
   case NotEqualOperator = "!="
   case AndOperator = "&&"
   case OrOperator = "||"
   case EqualsOperator = "="
   case BitwiseOrOperator = "|"
   case BitwiseAndOperator = "&"
   case ComplementOperator = "!"
   case BOF = "BOF"
   case EOF = "EOF"
   case Identifier = "id"
   case IntegerLiteral = "integerliteral"
   case BooleanLiteral = "booleanliteral"
   case CharacterLiteral = "characterliteral"
   case StringLiteral = "stringliteral"
   case NullLiteral = "nullliteral"
   case ByteKeyword = "byte"
   case ShortKeyword = "short"
   case IntKeyword = "int"
   case CharKeyword = "char"
   case BooleanKeyword = "boolean"
   case Semicolon = ";"
   case Comma = ","
   case LeftBrace = "{"
   case RightBrace = "}"
   case LeftBracket = "["
   case RightBracket = "]"
   case LeftParenthesis = "("
   case RightParenthesis = ")"
   case ReturnKeyword = "return"
   case ForKeyword = "for"
   case IfKeyword = "if"
   case ElseKeyword = "else"
   case WhileKeyword = "while"
   case ClassKeyword = "class"
   case NativeKeyword = "native"
   case StaticKeyword = "static"
   case AbstractKeyword = "abstract"
   case ProtectedKeyword = "protected"
   case PackageKeyword = "package"
   case PublicKeyword = "public"
   case ImportKeyword = "import"
   case VoidKeyword = "void"
   case ImplementsKeyword = "implements"
   case ExtendsKeyword = "extends"
   case InterfaceKeyword = "interface"
   case FinalKeyword = "final"

   /* TODO: non-terminals */

   public var isTerminal: Bool {
      switch self {
      case .ThisKeyword,
           .NewKeyword,
           .SubtractionOperator,
           .MultiplicationOperator,
           .Period,
           .DivisionOperator,
           .ModulusOperator,
           .AdditionOperator,
           .GreaterThanOperator,
           .LessThanOperator,
           .GreaterThanEqualOperator,
           .LessThanEqualsOperator,
           .InstanceofKeyword,
           .EqualsEqualsOperator,
           .NotEqualOperator,
           .AndOperator,
           .OrOperator,
           .EqualsOperator,
           .BitwiseOrOperator,
           .BitwiseAndOperator,
           .ComplementOperator,
           .BOF,
           .EOF,
           .Identifier,
           .IntegerLiteral,
           .BooleanLiteral,
           .CharacterLiteral,
           .StringLiteral,
           .NullLiteral,
           .ByteKeyword,
           .ShortKeyword,
           .IntKeyword,
           .CharKeyword,
           .BooleanKeyword,
           .Semicolon,
           .Comma,
           .LeftBrace,
           .RightBrace,
           .LeftBracket,
           .RightBracket,
           .LeftParenthesis,
           .RightParenthesis,
           .ReturnKeyword,
           .ForKeyword,
           .IfKeyword,
           .ElseKeyword,
           .WhileKeyword,
           .ClassKeyword,
           .NativeKeyword,
           .StaticKeyword,
           .AbstractKeyword,
           .ProtectedKeyword,
           .PackageKeyword,
           .PublicKeyword,
           .ImportKeyword,
           .VoidKeyword,
           .ImplementsKeyword,
           .ExtendsKeyword,
           .InterfaceKeyword,
           .FinalKeyword:
         return true
      }
   }

   public var isNonTerminal: Bool {
      return !isTerminal
   }
}
struct ParseTable {
   struct ProductionRule {
      let lhs: Symbol
      let rhs: [Symbol]
   }

   struct State {
      let lookaheadToProductionRuleMap: [Symbol: ProductionRule]
      let lookaheadToNextStateMap: [Symbol: Int]
   }

   let states: [State]
   let startState: State

   init(states: [State], startStateIndex: Int) {
      precondition(!states.isEmpty)

      self.states = states
      self.startState = states[startStateIndex]
   }

   func productionRuleForCurrentState(state: State, lookahead: Symbol) -> ProductionRule? {
      return state.lookaheadToProductionRuleMap[lookahead]
   }

   func nextStateForCurrentState(state: State, lookahead: Symbol) -> State? {
      guard let nextStateIndex = state.lookaheadToNextStateMap[lookahead] else {
         return nil
      }

      return states[nextStateIndex]
   }
}
struct Joos1W {
   static func f() {
      var _ = [
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "BOF")!: 196
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "abstract")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "compilationunit")!, rhs: [Symbol(rawValue: "BOF")!, Symbol(rawValue: "packagedeclaration")!, Symbol(rawValue: "importdeclaration")!, Symbol(rawValue: "EOF")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "public")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "short")!: 163
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "]")!: 244
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "andexpression")!, rhs: [Symbol(rawValue: "equalityexpression")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "==")!: 241,
               Symbol(rawValue: "!=")!: 206
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "booleanliteral")!: 202
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "fieldaccess")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "type")!, rhs: [Symbol(rawValue: "referencetype")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "(")!: 172,
               Symbol(rawValue: "[")!: 179
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "arraycreationexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "expressionstatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "protected")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "numerictype")!, rhs: [Symbol(rawValue: "integraltype")!]),
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "numerictype")!, rhs: [Symbol(rawValue: "integraltype")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "numerictype")!, rhs: [Symbol(rawValue: "integraltype")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "type")!, rhs: [Symbol(rawValue: "primitivetype")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "[")!: 5
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classbodydeclaration")!, rhs: [Symbol(rawValue: "constructordeclaration")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "constructorbody")!: 346
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "classinstancecreationexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "unaryexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "whilestatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "(")!: 61
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "assignmentexpression")!, rhs: [Symbol(rawValue: "assignment")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "assignmentexpression")!, rhs: [Symbol(rawValue: "assignment")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "assignmentexpression")!, rhs: [Symbol(rawValue: "assignment")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "assignmentexpression")!, rhs: [Symbol(rawValue: "assignment")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "interfacedeclaration")!, rhs: [Symbol(rawValue: "interface")!, Symbol(rawValue: "id")!, Symbol(rawValue: "extendsinterfaces")!, Symbol(rawValue: "interfacebody")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "statementwithouttrailingsubstatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "id")!: 344
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statement")!, rhs: [Symbol(rawValue: "forstatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "primary")!: 55,
               Symbol(rawValue: "castexpression")!: 58,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "classinstancecreationexpression")!: 19,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "unaryexpressionnotplusminus")!: 126,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "!")!: 33,
               Symbol(rawValue: "name")!: 140,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "-")!: 28,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "methodinvocation")!: 86,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "postfixexpression")!: 124,
               Symbol(rawValue: "unaryexpression")!: 314,
               Symbol(rawValue: "(")!: 32
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "this")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "arrayaccess")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "returnstatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "byte")!: 4,
               Symbol(rawValue: "equalityexpression")!: 6,
               Symbol(rawValue: "andexpression")!: 142,
               Symbol(rawValue: "relationalexpression")!: 35,
               Symbol(rawValue: "expression")!: 112,
               Symbol(rawValue: "assignmentexpression")!: 81,
               Symbol(rawValue: "primitivetype")!: 294,
               Symbol(rawValue: "conditionalandexpression")!: 88,
               Symbol(rawValue: "name")!: 59,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "|")!: 120,
               Symbol(rawValue: "assignment")!: 23,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "conditionalexpression")!: 115,
               Symbol(rawValue: "exclusiveorexpression")!: 75,
               Symbol(rawValue: "inclusiveorexpression")!: 82,
               Symbol(rawValue: "conditionalorexpression")!: 125,
               Symbol(rawValue: "numerictype")!: 65
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "primary")!: 55,
               Symbol(rawValue: "castexpression")!: 58,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "unaryexpressionnotplusminus")!: 126,
               Symbol(rawValue: "classinstancecreationexpression")!: 19,
               Symbol(rawValue: "!")!: 33,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "name")!: 140,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "-")!: 28,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "methodinvocation")!: 86,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "postfixexpression")!: 124,
               Symbol(rawValue: "unaryexpression")!: 255,
               Symbol(rawValue: "(")!: 32
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ";")!: 341
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "equalityexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ">=")!: 117,
               Symbol(rawValue: ">")!: 106,
               Symbol(rawValue: "<")!: 107,
               Symbol(rawValue: "instanceof")!: 37,
               Symbol(rawValue: "<=")!: 79
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "castexpression")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "primitivetype")!, Symbol(rawValue: ")")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "byte")!: 4,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "referencetype")!: 147,
               Symbol(rawValue: "primitivetype")!: 66,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "arraytype")!: 174,
               Symbol(rawValue: "name")!: 91,
               Symbol(rawValue: "classorinterfacetype")!: 99,
               Symbol(rawValue: "numerictype")!: 65
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ";")!: 138
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "equalityexpression")!: 6,
               Symbol(rawValue: "andexpression")!: 142,
               Symbol(rawValue: "relationalexpression")!: 35,
               Symbol(rawValue: "expression")!: 204,
               Symbol(rawValue: "assignmentexpression")!: 81,
               Symbol(rawValue: "conditionalandexpression")!: 88,
               Symbol(rawValue: "assignment")!: 23,
               Symbol(rawValue: "|")!: 120,
               Symbol(rawValue: "conditionalexpression")!: 115,
               Symbol(rawValue: "exclusiveorexpression")!: 75,
               Symbol(rawValue: "argumentlist")!: 226,
               Symbol(rawValue: "inclusiveorexpression")!: 82,
               Symbol(rawValue: "conditionalorexpression")!: 125,
               Symbol(rawValue: ")")!: 312
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "numerictype")!: 65,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "primitivetype")!: 247,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "classtype")!: 333,
               Symbol(rawValue: "name")!: 91,
               Symbol(rawValue: "classorinterfacetype")!: 116,
               Symbol(rawValue: "byte")!: 4
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifiers")!, rhs: [Symbol(rawValue: "modifier")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "interfacedeclaration")!, rhs: [Symbol(rawValue: "interface")!, Symbol(rawValue: "id")!, Symbol(rawValue: "interfacebody")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "boolean")!]),
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "boolean")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "boolean")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexprs")!, Symbol(rawValue: "dimexpr")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "(")!: 211
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexprs")!, rhs: [Symbol(rawValue: "dimexpr")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "abstract")!: 1,
               Symbol(rawValue: "native")!: 54,
               Symbol(rawValue: "public")!: 3,
               Symbol(rawValue: "modifier")!: 160,
               Symbol(rawValue: "interface")!: 84,
               Symbol(rawValue: "final")!: 146,
               Symbol(rawValue: "protected")!: 13,
               Symbol(rawValue: "static")!: 105,
               Symbol(rawValue: "class")!: 326
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "(")!: 273
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "]")!: 149
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "byte")!: 4,
               Symbol(rawValue: "void")!: 165,
               Symbol(rawValue: "public")!: 3,
               Symbol(rawValue: "modifier")!: 160,
               Symbol(rawValue: "primitivetype")!: 16,
               Symbol(rawValue: "name")!: 91,
               Symbol(rawValue: "classorinterfacetype")!: 99,
               Symbol(rawValue: "native")!: 54,
               Symbol(rawValue: "abstract")!: 1,
               Symbol(rawValue: "type")!: 180,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "referencetype")!: 9,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "constructordeclarator")!: 188,
               Symbol(rawValue: "arraytype")!: 174,
               Symbol(rawValue: "final")!: 146,
               Symbol(rawValue: "static")!: 105,
               Symbol(rawValue: "protected")!: 13,
               Symbol(rawValue: "numerictype")!: 65
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "variabledeclarators")!, rhs: [Symbol(rawValue: "variabledeclarator")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "variabledeclarator")!, rhs: [Symbol(rawValue: "variabledeclaratorid")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "=")!: 171
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "literal")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "modifier")!, rhs: [Symbol(rawValue: "native")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "postfixexpression")!, rhs: [Symbol(rawValue: "primary")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ".")!: 26
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementexpression")!, rhs: [Symbol(rawValue: "methodinvocation")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primary")!, rhs: [Symbol(rawValue: "primarynonewarray")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "[")!: 282
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "unaryexpressionnotplusminus")!, rhs: [Symbol(rawValue: "castexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "[")!: 49,
               Symbol(rawValue: "dims")!: 260
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "variabledeclaratorid")!: 52,
               Symbol(rawValue: "variabledeclarator")!: 51,
               Symbol(rawValue: "id")!: 168,
               Symbol(rawValue: "variabledeclarators")!: 190
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "relationalexpression")!: 35,
               Symbol(rawValue: "andexpression")!: 142,
               Symbol(rawValue: "assignment")!: 23,
               Symbol(rawValue: "expression")!: 271,
               Symbol(rawValue: "assignmentexpression")!: 81,
               Symbol(rawValue: "|")!: 120,
               Symbol(rawValue: "conditionalexpression")!: 115,
               Symbol(rawValue: "conditionalandexpression")!: 88,
               Symbol(rawValue: "exclusiveorexpression")!: 75,
               Symbol(rawValue: "inclusiveorexpression")!: 82,
               Symbol(rawValue: "conditionalorexpression")!: 125,
               Symbol(rawValue: "equalityexpression")!: 6
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "block")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "variabledeclaratorid")!: 52,
               Symbol(rawValue: "variabledeclarators")!: 225,
               Symbol(rawValue: "variabledeclarator")!: 51,
               Symbol(rawValue: "id")!: 145,
               Symbol(rawValue: "methoddeclarator")!: 275
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ";")!: 176
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "numerictype")!]),
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "numerictype")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primitivetype")!, rhs: [Symbol(rawValue: "numerictype")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "[")!: 5
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ";")!: 293
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forstatement")!, rhs: [Symbol(rawValue: "for")!, Symbol(rawValue: "(")!, Symbol(rawValue: ";")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ";")!, Symbol(rawValue: "forupdate")!, Symbol(rawValue: ")")!, Symbol(rawValue: "statement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "byte")!: 4,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "whilestatement")!: 21,
               Symbol(rawValue: "statementwithouttrailingsubstatement")!: 25,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "primitivetype")!: 16,
               Symbol(rawValue: "name")!: 132,
               Symbol(rawValue: "ifthenstatement")!: 158,
               Symbol(rawValue: "expressionstatement")!: 12,
               Symbol(rawValue: "forstatement")!: 27,
               Symbol(rawValue: "localvariabledeclaration")!: 38,
               Symbol(rawValue: "}")!: 94,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "type")!: 60,
               Symbol(rawValue: "block")!: 62,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "statementexpression")!: 34,
               Symbol(rawValue: "localvariabledeclarationstatement")!: 152,
               Symbol(rawValue: "assignment")!: 141,
               Symbol(rawValue: "statement")!: 173,
               Symbol(rawValue: "ifthenelsestatement")!: 137,
               Symbol(rawValue: "(")!: 78,
               Symbol(rawValue: "if")!: 22,
               Symbol(rawValue: "{")!: 69,
               Symbol(rawValue: "primary")!: 118,
               Symbol(rawValue: "numerictype")!: 65,
               Symbol(rawValue: "blockstatement")!: 181,
               Symbol(rawValue: "returnstatement")!: 31,
               Symbol(rawValue: "classinstancecreationexpression")!: 15,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "blockstatements")!: 87,
               Symbol(rawValue: "for")!: 48,
               Symbol(rawValue: "emptystatement")!: 72,
               Symbol(rawValue: "classorinterfacetype")!: 99,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "return")!: 92,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "referencetype")!: 9,
               Symbol(rawValue: "methodinvocation")!: 56,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "arraytype")!: 174,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: ";")!: 74
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "primary")!: 118,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "classinstancecreationexpression")!: 15,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "name")!: 10,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "assignment")!: 141,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "methodinvocation")!: 56,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "statementexpression")!: 322,
               Symbol(rawValue: "(")!: 210
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "relationalexpression")!, rhs: [Symbol(rawValue: "relationalexpression")!, Symbol(rawValue: "<=")!, Symbol(rawValue: "additiveexpression")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "-")!: 189,
               Symbol(rawValue: "+")!: 187
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "statementwithouttrailingsubstatement")!, rhs: [Symbol(rawValue: "emptystatement")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ")")!: 329
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "emptystatement")!, rhs: [Symbol(rawValue: ";")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "inclusiveorexpression")!, rhs: [Symbol(rawValue: "exclusiveorexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forinit")!, rhs: [Symbol(rawValue: "statementexpressionlist")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ",")!: 70
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "typedeclaration")!, rhs: [Symbol(rawValue: "interfacedeclaration")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "andexpression")!: 142,
               Symbol(rawValue: "assignment")!: 23,
               Symbol(rawValue: "|")!: 120,
               Symbol(rawValue: "expression")!: 73,
               Symbol(rawValue: "assignmentexpression")!: 81,
               Symbol(rawValue: "relationalexpression")!: 35,
               Symbol(rawValue: "conditionalexpression")!: 115,
               Symbol(rawValue: "conditionalandexpression")!: 88,
               Symbol(rawValue: "exclusiveorexpression")!: 75,
               Symbol(rawValue: "inclusiveorexpression")!: 82,
               Symbol(rawValue: "conditionalorexpression")!: 125,
               Symbol(rawValue: "equalityexpression")!: 6
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "additiveexpression")!: 71,
               Symbol(rawValue: "primary")!: 55,
               Symbol(rawValue: "castexpression")!: 58,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "classinstancecreationexpression")!: 19,
               Symbol(rawValue: "unaryexpressionnotplusminus")!: 126,
               Symbol(rawValue: "!")!: 33,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "name")!: 140,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "-")!: 28,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "methodinvocation")!: 86,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "multiplicativeexpression")!: 80,
               Symbol(rawValue: "unaryexpression")!: 20,
               Symbol(rawValue: "postfixexpression")!: 124,
               Symbol(rawValue: "(")!: 32
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "additiveexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "/")!: 130,
               Symbol(rawValue: "%")!: 127,
               Symbol(rawValue: "*")!: 128
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "expression")!, rhs: [Symbol(rawValue: "assignmentexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "expression")!, rhs: [Symbol(rawValue: "assignmentexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "expression")!, rhs: [Symbol(rawValue: "assignmentexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "expression")!, rhs: [Symbol(rawValue: "assignmentexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalandexpression")!, rhs: [Symbol(rawValue: "inclusiveorexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "id")!: 269
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "id")!: 237
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classdeclaration")!, rhs: [Symbol(rawValue: "class")!, Symbol(rawValue: "id")!, Symbol(rawValue: "interfaces")!, Symbol(rawValue: "classbody")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "methodinvocation")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ";")!: 74,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "whilestatement")!: 21,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "primitivetype")!: 16,
               Symbol(rawValue: "statementwithouttrailingsubstatement")!: 25,
               Symbol(rawValue: "name")!: 132,
               Symbol(rawValue: "ifthenstatement")!: 158,
               Symbol(rawValue: "localvariabledeclarationstatement")!: 152,
               Symbol(rawValue: "expressionstatement")!: 12,
               Symbol(rawValue: "localvariabledeclaration")!: 38,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "forstatement")!: 27,
               Symbol(rawValue: "type")!: 60,
               Symbol(rawValue: "block")!: 62,
               Symbol(rawValue: "assignment")!: 141,
               Symbol(rawValue: "statementexpression")!: 34,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "}")!: 332,
               Symbol(rawValue: "statement")!: 173,
               Symbol(rawValue: "ifthenelsestatement")!: 137,
               Symbol(rawValue: "returnstatement")!: 31,
               Symbol(rawValue: "if")!: 22,
               Symbol(rawValue: "numerictype")!: 65,
               Symbol(rawValue: "primary")!: 118,
               Symbol(rawValue: "{")!: 69,
               Symbol(rawValue: "blockstatement")!: 194,
               Symbol(rawValue: "(")!: 78,
               Symbol(rawValue: "classinstancecreationexpression")!: 15,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "for")!: 48,
               Symbol(rawValue: "emptystatement")!: 72,
               Symbol(rawValue: "classorinterfacetype")!: 99,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "return")!: 92,
               Symbol(rawValue: "integraltype")!: 14,
               Symbol(rawValue: "referencetype")!: 9,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: "boolean")!: 43,
               Symbol(rawValue: "methodinvocation")!: 56,
               Symbol(rawValue: "arraytype")!: 174,
               Symbol(rawValue: "byte")!: 4
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalorexpression")!, rhs: [Symbol(rawValue: "conditionalandexpression")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalorexpression")!, rhs: [Symbol(rawValue: "conditionalandexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalorexpression")!, rhs: [Symbol(rawValue: "conditionalandexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalorexpression")!, rhs: [Symbol(rawValue: "conditionalandexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "conditionalorexpression")!, rhs: [Symbol(rawValue: "conditionalandexpression")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "&&")!: 167
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "compilationunit")!, rhs: [Symbol(rawValue: "BOF")!, Symbol(rawValue: "typedeclaration")!, Symbol(rawValue: "EOF")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "dimexpr")!, rhs: [Symbol(rawValue: "[")!, Symbol(rawValue: "expression")!, Symbol(rawValue: "]")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "classorinterfacetype")!, rhs: [Symbol(rawValue: "name")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "equalityexpression")!: 6,
               Symbol(rawValue: "relationalexpression")!: 35,
               Symbol(rawValue: "andexpression")!: 142,
               Symbol(rawValue: "expression")!: 217,
               Symbol(rawValue: "assignmentexpression")!: 81,
               Symbol(rawValue: "conditionalandexpression")!: 88,
               Symbol(rawValue: "assignment")!: 23,
               Symbol(rawValue: "|")!: 120,
               Symbol(rawValue: "conditionalexpression")!: 115,
               Symbol(rawValue: "exclusiveorexpression")!: 75,
               Symbol(rawValue: "inclusiveorexpression")!: 82,
               Symbol(rawValue: "conditionalorexpression")!: 125,
               Symbol(rawValue: ";")!: 178
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "whilestatement")!: 21,
               Symbol(rawValue: "this")!: 29,
               Symbol(rawValue: "statementwithouttrailingsubstatement")!: 25,
               Symbol(rawValue: "integerliteral")!: 7,
               Symbol(rawValue: "name")!: 10,
               Symbol(rawValue: "ifthenstatement")!: 158,
               Symbol(rawValue: "expressionstatement")!: 12,
               Symbol(rawValue: "forstatement")!: 27,
               Symbol(rawValue: "new")!: 40,
               Symbol(rawValue: "primarynonewarray")!: 57,
               Symbol(rawValue: "block")!: 62,
               Symbol(rawValue: "assignment")!: 141,
               Symbol(rawValue: "statementexpression")!: 34,
               Symbol(rawValue: "statement")!: 148,
               Symbol(rawValue: "ifthenelsestatement")!: 137,
               Symbol(rawValue: "returnstatement")!: 31,
               Symbol(rawValue: "if")!: 22,
               Symbol(rawValue: "{")!: 69,
               Symbol(rawValue: "primary")!: 118,
               Symbol(rawValue: "(")!: 78,
               Symbol(rawValue: "classinstancecreationexpression")!: 15,
               Symbol(rawValue: "statementnoshortif")!: 119,
               Symbol(rawValue: "fieldaccess")!: 8,
               Symbol(rawValue: "for")!: 48,
               Symbol(rawValue: "emptystatement")!: 72,
               Symbol(rawValue: "arraycreationexpression")!: 11,
               Symbol(rawValue: "return")!: 92,
               Symbol(rawValue: "arrayaccess")!: 30,
               Symbol(rawValue: "methodinvocation")!: 56,
               Symbol(rawValue: "literal")!: 53,
               Symbol(rawValue: ";")!: 74
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "if")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "void")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "integerliteral")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "this")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "for")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "new")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "}")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "return")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "boolean")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "(")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "{")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!]),
               Symbol(rawValue: "byte")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "block")!, rhs: [Symbol(rawValue: "{")!, Symbol(rawValue: "}")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "forupdate")!, rhs: [Symbol(rawValue: "statementexpressionlist")!])
            ],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ",")!: 70
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "*")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "/")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "+")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "-")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: "%")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "multiplicativeexpression")!, rhs: [Symbol(rawValue: "multiplicativeexpression")!, Symbol(rawValue: "*")!, Symbol(rawValue: "unaryexpression")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: ")")!: 306
            ]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "abstract")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "native")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "public")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "interface")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "final")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "protected")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "class")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: "static")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!]),
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "importdeclaration")!, rhs: [Symbol(rawValue: "singletypeimportdeclaration")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: ";")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: ">=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "id")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: ">")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "!=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "instanceof")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "<")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "<=")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: ",")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "]")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "==")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "&&")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: "||")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!]),
               Symbol(rawValue: ")")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "referencetype")!, rhs: [Symbol(rawValue: "classorinterfacetype")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "[")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ")")!]),
               Symbol(rawValue: ".")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "primarynonewarray")!, rhs: [Symbol(rawValue: "(")!, Symbol(rawValue: "expression")!, Symbol(rawValue: ")")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [
               Symbol(rawValue: "EOF")!: ParseTable.ProductionRule(lhs: Symbol(rawValue: "typedeclaration")!, rhs: [Symbol(rawValue: "classdeclaration")!])
            ],
            lookaheadToNextStateMap: [:]
         ),
         ParseTable.State(
            lookaheadToProductionRuleMap: [:],
            lookaheadToNextStateMap: [
               Symbol(rawValue: "id")!: 45,
               Symbol(rawValue: "methoddeclarator")!: 298
            ]
         )
      ]
   }
}
