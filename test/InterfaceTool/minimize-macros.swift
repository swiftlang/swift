// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-interface-tool -action minimize %t/basic/input.swift > %t/basic/output.swift
// RUN: %diff -u %t/basic/expected.swift %t/basic/output.swift

// RUN: %swift-interface-tool -action minimize %t/with-attributes/input.swift > %t/with-attributes/output.swift
// RUN: %diff -u %t/with-attributes/expected.swift %t/with-attributes/output.swift

// RUN: %swift-interface-tool -action minimize %t/non-external-dropped/input.swift > %t/non-external-dropped/output.swift
// RUN: %diff -u %t/non-external-dropped/expected.swift %t/non-external-dropped/output.swift

// RUN: %swift-interface-tool -action minimize %t/in-ifconfig/input.swift > %t/in-ifconfig/output.swift
// RUN: %diff -u %t/in-ifconfig/expected.swift %t/in-ifconfig/output.swift

// RUN: %swift-interface-tool -action minimize %t/string-with-comment-chars/input.swift > %t/string-with-comment-chars/output.swift
// RUN: %diff -u %t/string-with-comment-chars/expected.swift %t/string-with-comment-chars/output.swift

// RUN: %swift-interface-tool -action minimize %t/ifconfig-in-attributes/input.swift > %t/ifconfig-in-attributes/output.swift
// RUN: %diff -u %t/ifconfig-in-attributes/expected.swift %t/ifconfig-in-attributes/output.swift

// Minimized output should parse.
// RUN: %target-swift-frontend -parse %t/with-attributes/output.swift
// RUN: %target-swift-frontend -parse %t/ifconfig-in-attributes/output.swift

//--- basic/input.swift
import AlphaMod

public macro MyMacro() = #externalMacro(module: "MacroPlugin", type: "MyMacroImpl")

public func unrelated() {}
//--- basic/expected.swift
import AlphaMod
public macro MyMacro() = #externalMacro(module: "MacroPlugin", type: "MyMacroImpl")
//--- with-attributes/input.swift
import BetaMod

@attached(member, names: named(init), named(Storage))
@attached(memberAttribute)
public macro MyMacro() = #externalMacro(
  module: "MacroPlugin",
  type: "MyMacroImpl"
)

public struct Foo {}
//--- with-attributes/expected.swift
import BetaMod
@attached(member, names: named(init), named(Storage)) @attached(memberAttribute) public macro MyMacro() = #externalMacro(module: "MacroPlugin", type: "MyMacroImpl")
//--- non-external-dropped/input.swift
import BetaMod

// Macro whose definition is a non-`#externalMacro` macro expansion: drop.
public macro Stringified<T>(_ value: T) = #stringify(value)

// Macro with no definition: drop.
public macro NoDef()
//--- non-external-dropped/expected.swift
import BetaMod
//--- in-ifconfig/input.swift
import BetaMod

#if compiler(>=5.9)
@attached(extension)
public macro OtherMacro() = #externalMacro(module: "MacroPlugin", type: "OtherMacroImpl")
#endif

#if compiler(>=5.9)
public func unrelatedAndDropped() {}
#endif
//--- in-ifconfig/expected.swift
import BetaMod
#if compiler(>=5.9)
@attached(extension) public macro OtherMacro() = #externalMacro(module: "MacroPlugin", type: "OtherMacroImpl")
#endif
//--- string-with-comment-chars/input.swift
import BetaMod

public macro Tricky() = #externalMacro(module: "Has//Slashes", type: "Has/*StarSlash*/Type")
//--- string-with-comment-chars/expected.swift
import BetaMod
public macro Tricky() = #externalMacro(module: "Has//Slashes", type: "Has/*StarSlash*/Type")
//--- ifconfig-in-attributes/input.swift
// An `#if` inside an attribute list is part of the declaration it is attached
// to, but a directive must start a line, so it is broken onto lines of its own
// while the attributes around it stay collapsed onto the declaration's line.
import BetaMod

@attached(member)
#if DEBUG
@attached(peer)
#endif
public macro Trailing() = #externalMacro(module: "M", type: "T")

// The `#if` may also lead the attribute list, or carry `#else` clauses.
#if canImport(CondMod)
@attached(peer)
#endif
public macro Leading() = #externalMacro(module: "M", type: "T")

@attached(member)
#if DEBUG
@attached(peer)
#else
@attached(extension)
#endif
@attached(memberAttribute)
public macro Middle() = #externalMacro(module: "M", type: "T")

// Attribute-list `#if`s on an import are kept with it.
@_documentation(visibility: internal)
#if FOO
@_spi(X)
#endif
import Attributed

// A `canImport` in an attribute list does not rescue an otherwise-dropped
// declaration — the attributes go wherever their declaration goes.
@available(macOS 14, *)
#if canImport(CondMod)
@MainActor
#endif
public func dropped() {}
//--- ifconfig-in-attributes/expected.swift
import BetaMod
@attached(member)
#if DEBUG
@attached(peer)
#endif
public macro Trailing() = #externalMacro(module: "M", type: "T")
#if canImport(CondMod)
@attached(peer)
#endif
public macro Leading() = #externalMacro(module: "M", type: "T")
@attached(member)
#if DEBUG
@attached(peer)
#else
@attached(extension)
#endif
@attached(memberAttribute) public macro Middle() = #externalMacro(module: "M", type: "T")
@_documentation(visibility: internal)
#if FOO
@_spi(X)
#endif
import Attributed
