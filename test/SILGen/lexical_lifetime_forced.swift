// RUN: %target-swift-emit-silgen -enable-lexical-lifetimes=false -module-name borrow -parse-stdlib %s | %FileCheck %s

// CHECK-LABEL: sil {{.*}}[lexical_lifetimes] [ossa] @funky : {{.*}} {
// CHECK-LABEL: } // end sil function 'funky'
@_silgen_name("funky")
@_lexicalLifetimes func funky() {}

