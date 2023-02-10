// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: %{python} %utils/split_file.py -o %t %s


// BEGIN MyModule.swift

public class UserCollection {
  public static let sharedStatic = UserCollection()
  public class var sharedComputedClass: UserCollection { UserCollection() }
}

// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name MyModule \
// RUN:     -emit-module-path %t/Modules/MyModule.swiftmodule \
// RUN:     -emit-module-doc-path %t/Modules/MyModule.swiftdoc \
// RUN:     %t/MyModule.swift

// BEGIN test.swift
import MyModule

func application() {
  UserCollection.sharedStatic
  UserCollection.sharedComputedClass
}

// RUN: %sourcekitd-test -req=cursor -pos=4:18 %t/test.swift -- %t/test.swift -I %t/Modules | %FileCheck %s --check-prefix=SHARED_STATIC

// FIXME: This should be reported as 'static var' rdar://105239467
// SHARED_STATIC: <Declaration>class let sharedStatic: <Type usr="s:8MyModule14UserCollectionC">UserCollection</Type></Declaration>
// SHARED_STATIC: <decl.var.class><syntaxtype.keyword>class</syntaxtype.keyword> <syntaxtype.keyword>let</syntaxtype.keyword> <decl.name>sharedStatic</decl.name>: <decl.var.type><ref.class usr="s:8MyModule14UserCollectionC">UserCollection</ref.class></decl.var.type></decl.var.class>

// RUN: %sourcekitd-test -req=cursor -pos=5:18 %t/test.swift -- %t/test.swift -I %t/Modules| %FileCheck %s --check-prefix=SHARED_COMPUTED_CLASS

// SHARED_COMPUTED_CLASS: <Declaration>class var sharedComputedClass: <Type usr="s:8MyModule14UserCollectionC">UserCollection</Type> { get }</Declaration>
// SHARED_COMPUTED_CLASS: <decl.var.class><syntaxtype.keyword>class</syntaxtype.keyword> <syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>sharedComputedClass</decl.name>: <decl.var.type><ref.class usr="s:8MyModule14UserCollectionC">UserCollection</ref.class></decl.var.type> { <syntaxtype.keyword>get</syntaxtype.keyword> }</decl.var.class>
