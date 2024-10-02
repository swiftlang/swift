@freestanding(declaration, names: named(MacroStruct))
public macro StructMacro() = #externalMacro(module: "TestMacro1Macros", type: "StructMacro")
