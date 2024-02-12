@freestanding(declaration, names: named(StructWithUnqualifiedLookup))
public macro structWithUnqualifiedLookup() = #externalMacro(module: "MacroDefinition", type: "DefineStructWithUnqualifiedLookupMacro")

@freestanding(declaration)
public macro anonymousTypes(public: Bool = false, causeErrors: Bool = false, _: () -> String) = #externalMacro(module: "MacroDefinition", type: "DefineAnonymousTypesMacro")

@freestanding(declaration)
public macro introduceTypeCheckingErrors() = #externalMacro(module: "MacroDefinition", type: "IntroduceTypeCheckingErrorsMacro")

@freestanding(declaration)
public macro freestandingWithClosure<T>(_ value: T, body: (T) -> T) = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")

@freestanding(declaration, names: arbitrary)
public macro bitwidthNumberedStructs(_ baseName: String) = #externalMacro(module: "MacroDefinition", type: "DefineBitwidthNumberedStructsMacro")

@freestanding(expression)
public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@freestanding(declaration, names: named(value))
public macro varValue() = #externalMacro(module: "MacroDefinition", type: "VarValueMacro")

// Macros that pass along generic arguments

@freestanding(expression)
public macro checkGeneric_root<DAS>() = #externalMacro(module: "MacroDefinition", type: "GenericToVoidMacro")
@freestanding(expression)
public macro checkGeneric<DAS>() = #checkGeneric_root<DAS>()

@freestanding(expression)
public macro checkGeneric2_root<A, B>() = #externalMacro(module: "MacroDefinition", type: "GenericToVoidMacro")
@freestanding(expression)
public macro checkGeneric2<A, B>() = #checkGeneric2_root<A, B>()

@freestanding(expression)
public macro checkGenericHashableCodable_root<A: Hashable, B: Codable>() = #externalMacro(module: "MacroDefinition", type: "GenericToVoidMacro")
@freestanding(expression)
public macro checkGenericHashableCodable<A: Hashable, B: Codable>() = #checkGenericHashableCodable_root<A, B>()
