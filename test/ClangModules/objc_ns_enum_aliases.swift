// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -import-ns-enum -parse -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -verify 

import Foundation

var alias1 = NSEnumWithAliases.BySameValue // expected-error{{}}
var alias2 = NSEnumWithAliases.ByEquivalentValue // expected-error{{}}
var alias3 = NSEnumWithAliases.ByName // expected-error{{}}

var aliasOriginal = NSEnumWithAliases.Original
