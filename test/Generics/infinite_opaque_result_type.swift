// RUN: %target-typecheck-verify-swift

func concrete1() -> some Any {
// expected-error@-1 {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}
  return concrete1()
}

func concrete2() -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [concrete2()]
}


func concrete1a() -> some Any {  // expected-error {{function opaque return type was inferred as 'some Any', which defines the opaque type in terms of itself}}
  return concrete1b()
}

func concrete1b() -> some Any {  // expected-error {{function opaque return type was inferred as 'some Any', which defines the opaque type in terms of itself}}
  return concrete1a()
}


func concrete2a() -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [concrete2b()]
}

func concrete2b() -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [concrete2a()]
}


func generic1<T>(_ t: T) -> some Any {
// expected-error@-1 {{function declares an opaque return type, but has no return statements in its body from which to infer an underlying type}}
  return generic1(t)
}

func generic2<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [generic2(t)]
}


func generic1a<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as 'some Any', which defines the opaque type in terms of itself}}
  return generic1b(t)
}

func generic1b<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as 'some Any', which defines the opaque type in terms of itself}}
  return generic1a(t)
}


func generic2a<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [generic2b(t)]
}

func generic2b<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [generic2a(t)]
}


func generic3a<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [generic3b(t)]
}

func generic3b<T>(_ t: T) -> some Any {  // expected-error {{function opaque return type was inferred as '[some Any]', which defines the opaque type in terms of itself}}
  return [generic3a([t])]
}

func very_wide1() -> some Any {  // expected-error {{function opaque return type was inferred as '(some Any, some Any)', which defines the opaque type in terms of itself}}
  return (very_wide2(), very_wide2())
}

func very_wide2() -> some Any {  // expected-error {{function opaque return type was inferred as '(some Any, some Any)', which defines the opaque type in terms of itself}}
  return (very_wide1(), very_wide1())
}