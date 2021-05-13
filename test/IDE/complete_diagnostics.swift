// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -I %t/Modules -module-name OtherModule -o %t/Modules %t/OtherModule.swift
// RUN: %target-swift-frontend -emit-module -I %t/Modules -module-name MyModule -o %t/Modules %t/MyModule.swift

// RUN: %empty-directory(%t/output)
// RUN: %empty-directory(%t/ccp)
// RUN: %empty-directory(%t/mcp)

// NOTE: Doing twice is to ensure that the completion cache is used.
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp

// BEGIN UnrelatedModule.swift

public func declInUnRelatedModule() {}

// BEGIN OtherModule.swift

public func declInOtherModule() {}

// BEGIN MyModule.swift

@_exported import OtherModule

public struct Foo {
  @available(*, deprecated)
  public func deprecatedUnconditional() {}

  @available(macOS, deprecated)
  public func deprecatedPlatformUnconditional() {}

  @available(macOS, deprecated: 10.4)
  public func deprecatedPlatformVersion() {}

  @available(macOS, deprecated: 10.4, message: "this is a \"message\"")
  public func deprecatedPlatformVersionMessage() {}

  @available(macOS, deprecated: 10.4, renamed: "renamedName")
  public func deprecatedPlatformVersionRenamed() {}

  @available(macOS, deprecated: 10.4, message: "this is a message", renamed: "renamedName")
  public func deprecatedPlatformVersionMessageRenamed() {}

  @available(swift, deprecated: 3.2)
  public var deprecatedVar: Int { 1 }
}

// BEGIN App.swift

import MyModule
import #^IMPORT^#;
// IMPORT: Begin completions
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   MyModule[#Module#]; name=MyModule; diagnostics=warning:module 'MyModule' is already imported{{$}}
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   OtherModule[#Module#]; name=OtherModule; diagnostics=note:module 'OtherModule' is already imported via another module import{{$}}
// IMPORT-DAG: Decl[Module]/None/NotRecommended:   Swift[#Module#]; name=Swift; diagnostics=warning:module 'Swift' is already imported{{$}}
// IMPORT: End completions

func test(foo: Foo) {
  foo.#^MEMBER^#
// MEMBER: Begin completions
// MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Foo#]; name=self
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedUnconditional()[#Void#]; name=deprecatedUnconditional(); diagnostics=warning:'deprecatedUnconditional()' is deprecated{{$}}
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedPlatformUnconditional()[#Void#]; name=deprecatedPlatformUnconditional(); diagnostics=warning:'deprecatedPlatformUnconditional()' is deprecated in macOS{{$}}
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedPlatformVersion()[#Void#]; name=deprecatedPlatformVersion(); diagnostics=warning:'deprecatedPlatformVersion()' was deprecated in macOS 10.4{{$}}
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedPlatformVersionMessage()[#Void#]; name=deprecatedPlatformVersionMessage(); diagnostics=warning:'deprecatedPlatformVersionMessage()' was deprecated in macOS 10.4: this is a "message"{{$}}
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedPlatformVersionRenamed()[#Void#]; name=deprecatedPlatformVersionRenamed(); diagnostics=warning:'deprecatedPlatformVersionRenamed()' was deprecated in macOS 10.4: renamed to 'renamedName'{{$}}
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedPlatformVersionMessageRenamed()[#Void#]; name=deprecatedPlatformVersionMessageRenamed(); diagnostics=warning:'deprecatedPlatformVersionMessageRenamed()' was deprecated in macOS 10.4: this is a message{{$}}
// MEMBER-DAG: Decl[InstanceVar]/CurrNominal/NotRecommended: deprecatedVar[#Int#]; name=deprecatedVar; diagnostics=warning:'deprecatedVar' is deprecated{{$}}
// MEMBER: End completions
}

func testOwnGetter() {
    var valueInOwnGetter: Int {
      #^GETTER^#
    }
// GETTER: Begin completions 
// GETTER-DAG: Decl[LocalVar]/Local/NotRecommended/TypeRelation[Identical]: valueInOwnGetter[#Int#]; name=valueInOwnGetter; diagnostics=warning:attempting to access 'valueInOwnGetter' within its own getter{{$}}
// GETTER: End completions
}
