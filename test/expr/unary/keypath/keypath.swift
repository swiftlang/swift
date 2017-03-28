// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library  %s -verify

func testKeyPath() {
  _ = #keyPath2(A, .property)
  _ = #keyPath2(A, [sub])
  _ = #keyPath2(A, .property?)
  _ = #keyPath2(A, .property!)
  _ = #keyPath2(A, .property[sub]?!?.property!?![sub])
  _ = #keyPath2([Array], .property)
  _ = #keyPath2([Array], [sub])
  _ = #keyPath2([Key: Value], [sub])
  _ = #keyPath2(Generic<T>, .value)
  _ = #keyPath2(() -> (), .ok)

  _ = #keyPath2(.property)
  _ = #keyPath2([sub])
  _ = #keyPath2(.property?)
  _ = #keyPath2(.property!)
  _ = #keyPath2(.property[sub]?!?.property!?![sub])
}

func testKeyPathErrors() { // expected-note{{}}
  // TODO: recovery
  _ = #keyPath2(.  ; // expected-error{{expected property or type name}}
  _ = #keyPath2(.a ;
  _ = #keyPath2([a ;
  _ = #keyPath2([a];
  _ = #keyPath2(?  ;
  _ = #keyPath2(!  ;
  _ = #keyPath2(.  ;
  _ = #keyPath2(.a ;
  _ = #keyPath2([a ;
  _ = #keyPath2([a,;
  _ = #keyPath2([a:;
  _ = #keyPath2([a];
  _ = #keyPath2(.a?;
  _ = #keyPath2(.a!;
  _ = #keyPath2(A     ;
  _ = #keyPath2(A,    ;
  _ = #keyPath2(A<    ;
  _ = #keyPath2(A, .  ;
  _ = #keyPath2(A, .a ;
  _ = #keyPath2(A, [a ;
  _ = #keyPath2(A, [a];
  _ = #keyPath2(A, ?  ;
  _ = #keyPath2(A, !  ;
  _ = #keyPath2(A, .  ;
  _ = #keyPath2(A, .a ;
  _ = #keyPath2(A, [a ;
  _ = #keyPath2(A, [a,;
  _ = #keyPath2(A, [a:;
  _ = #keyPath2(A, [a];
  _ = #keyPath2(A, .a?;
  _ = #keyPath2(A, .a!;
} // expected-error@+1{{}}
