# libSyntax nodes status

## Expression

### Specialized:
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

### In-progress (UnknownExpr):
  * InterpolatedStringLiteralExpr
  * ObjectLiteralExpr
  * MagicIdentifierLiteralExpr
  * CallExpr
  * UnresolvedDotExpr
  * InOutExpr
  * KeyPathExpr
  * KeyPathDotExpr
  * EditorPlaceholderExpr

### Not-specialized (UnknownExpr):
  * SuperRefExpr
  * UnresolvedSpecializeExpr
  * DotSelfExpr
  * SubscriptExpr
  * KeyPathApplicationExpr
  * CaptureListExpr
  * AutoClosureExpr
  * DynamicTypeExpr
  * BindOptionalExpr
  * OptionalEvaluationExpr
  * ForceValueExpr
  * PostfixUnaryExpr
  * ForcedCheckedCastExpr
  * ConditionalCheckedCastExpr
  * IsExpr
  * CoerceExpr
  * ArrowExpr
  * UnresolvedPatternExpr
  * ObjCSelectorExpr

## Declaration

### Specialized:
  * TopLevelCodeDecl
  * StructDecl
  * FuncDecl
  * ProtocolDecl
  * ImportDecl

### In-progress (UnknownDecl):
  * TypeAliasDecl
  * PatternBindingDecl
  * VarDecl
  * IfConfigDecl
  * ClassDecl (SR-6571)
  * ExtensionDecl (SR-6572)

### Not-specialized (UnknownDecl):
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
### Specialized:
  * BraceStmt
  * ReturnStmt

### Not-specialized (UnknownStmt):
  * DeferStmt
  * IfStmt
  * GuardStmt
  * WhileStmt
  * DoStmt
  * DoCatchStmt
  * RepeatWhileStmt
  * ForEachStmt
  * SwitchStmt
  * CaseStmt
  * CatchStmt
  * BreakStmt
  * ContinueStmt
  * FallthroughStmt
  * FailStmt
  * ThrowStmt

## Pattern
### Not-specialized:
  * ParenPattern
  * TuplePattern
  * NamedPattern
  * AnyPattern
  * TypedPattern
  * VarPattern


## TypeRepr
  * To-be filled
