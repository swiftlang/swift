// RUN: %swift -emit-silgen -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

func foo() {
  // CHECK-LABEL: sil private [transparent] @_TToFC10objc_localL33_A955410181627128E3785E314285BB18_3Foog1xSi
  @objc class Foo { @objc var x: Int = 0 }
}
