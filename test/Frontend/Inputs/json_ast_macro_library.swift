@attached(member, names: named(_macroInjectedMember))
public macro InjectMember() = #externalMacro(module: "MacroDefinition", type: "MemberInjectingMacro")
