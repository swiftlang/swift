# libSyntax nodes status

## Expression

### Done:
  * NilLiteralExpr
  * IntegerLiteralExpr
  * FloatLiteralExpr
  * BooleanLiteralExpr
  * StringLiteralExpr
  * DiscardAssignmentExpr
  * DeclRefExpr
  * IfExpr
  * AssignExpr
  * TypeExpr
  * UnresolvedMemberExpr
  * SequenceExpr
  * TupleElementExpr
  * TupleExpr
  * ArrayExpr
  * DictionaryExpr
  * PrefixUnaryExpr
  * TryExpr
  * ForceTryExpr
  * OptionalTryExpr
  * ClosureExpr
  * FunctionCallExpr
  * SubscriptExpr
  * PostfixUnaryExpr
  * ForcedValueExpr
  * SuperRefExpr
  * ImplicitMemberExpr

### In-progress (UnknownExpr):
  * InterpolatedStringLiteralExpr
  * ObjectLiteralExpr
  * MagicIdentifierLiteralExpr
  * InOutExpr
  * KeyPathExpr
  * KeyPathDotExpr
  * EditorPlaceholderExpr

### Not-started (UnknownExpr):
  * UnresolvedSpecializeExpr
  * DotSelfExpr
  * KeyPathApplicationExpr
  * CaptureListExpr
  * AutoClosureExpr
  * DynamicTypeExpr
  * ForcedCheckedCastExpr
  * ConditionalCheckedCastExpr
  * IsExpr
  * CoerceExpr
  * ArrowExpr
  * UnresolvedPatternExpr
  * ObjCSelectorExpr

## Declaration

### Done:
  * TopLevelCodeDecl
  * ClassDecl
  * StructDecl
  * FuncDecl
  * ProtocolDecl
  * ImportDecl
  * TypeAliasDecl
  * IfConfigDecl
  * PatternBindingDecl
  * VarDecl
  * ExtensionDecl

### In-progress (UnknownDecl):

### Not-started (UnknownDecl):
  * EnumCaseDecl
  * PrecedenceGroupDecl
  * InfixOperatorDecl
  * PrefixOperatorDecl
  * PostfixOperatorDecl
  * AssociatedTypeDecl
  * EnumDecl
  * SubscriptDecl
  * ConstructorDecl
  * DestructorDecl
  * EnumElementDecl

## Statement
### Done:
  * BraceStmt
  * ReturnStmt
  * DeferStmt
  * DoStmt
  * RepeatWhileStmt
  * BreakStmt
  * ContinueStmt
  * FallthroughStmt
  * ThrowStmt
  * IfStmt
  * GuardStmt
  * WhileStmt
  * ForInStmt

### Not-started (UnknownStmt):
  * SwitchStmt

## Pattern
### Done:
  * IdentifierPattern
  * WildcardPattern
  * TuplePattern
  * ExpressionPattern
  * ValueBindingPattern
  * IsTypePattern

### Not-started:
  * AsTypePattern
  * OptionalPattern
  * EnumCasePattern

## TypeRepr
### Done:
  * SimpleTypeIdentifier
  * MemberTypeIdentifier
  * ArrayType
  * DictionaryType
  * MetatypeType
  * OptionalType
  * ImplicitlyUnwrappedOptionalType
  * CompositionType
  * TupleType
  * FunctionType
  * AttributedType
