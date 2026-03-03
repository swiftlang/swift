@attached(member, names: named(_macroInjectedMember))
public macro InjectMember() = #externalMacro(module: "MacroDefinition", type: "MemberInjectingMacro")

@attached(peer, names: named(FixedNamePeer))
public macro InjectPeer() = #externalMacro(module: "MacroDefinition", type: "PeerInjectingMacro")

@freestanding(declaration, names: named(FixedNameFreestander))
public macro injectFreestanding() = #externalMacro(module: "MacroDefinition", type: "FreestandingInjectingMacro")
