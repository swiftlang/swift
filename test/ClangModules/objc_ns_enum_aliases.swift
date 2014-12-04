// RUN: %swift %clang-importer-sdk -parse -target x86_64-apple-macosx10.9 %s -verify

import Foundation

var alias1 = NSAliasesEnum.BySameValue // expected-error{{}}
var alias2 = NSAliasesEnum.ByEquivalentValue // expected-error{{}}
var alias3 = NSAliasesEnum.ByName // expected-error{{}}

var aliasOriginal = NSAliasesEnum.Original

var qualifiedName = NSRuncingMode.Mince
var topLevelCaseName = NSRuncingMince // expected-error{{}}
