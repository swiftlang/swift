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

### Not-started (UnknownExpr):
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

### Done:
  * TopLevelCodeDecl
  * ClassDecl
  * StructDecl
  * FuncDecl
  * ProtocolDecl
  * ImportDecl
  * TypeAliasDecl
  * IfConfigDecl

### In-progress (UnknownDecl):
  * PatternBindingDecl
  * VarDecl
  * ExtensionDecl (SR-6572)

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

### Not-started (UnknownStmt):
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
### Not-started:
  * ParenPattern
  * TuplePattern
  * NamedPattern
  * AnyPattern
  * TypedPattern
  * VarPattern


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

### In-progress:
  * AttributedType
