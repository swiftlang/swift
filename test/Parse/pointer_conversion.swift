// RUN: %target-parse-verify-swift

// XFAIL: linux

class C {}
class D {}

func takesMutablePointer(x: UnsafeMutablePointer<Int>) {}
func takesMutableVoidPointer(x: UnsafeMutablePointer<Void>) {}
func takesMutableInt8Pointer(x: UnsafeMutablePointer<Int8>) {} 
func takesMutableArrayPointer(x: UnsafeMutablePointer<[Int]>) {} 
func takesConstPointer(x: UnsafePointer<Int>) -> Character { return "x" }
func takesConstInt8Pointer(x: UnsafePointer<Int8>) {}
func takesConstUInt8Pointer(x: UnsafePointer<UInt8>) {}
func takesConstVoidPointer(x: UnsafePointer<Void>) {}
func takesAutoreleasingPointer(x: AutoreleasingUnsafeMutablePointer<C>) {} 

func mutablePointerArguments(p: UnsafeMutablePointer<Int>,
                             cp: UnsafePointer<Int>,
                             ap: AutoreleasingUnsafeMutablePointer<Int>) {
  takesMutablePointer(nil)
  takesMutablePointer(p)
  takesMutablePointer(cp) // expected-error{{cannot convert value of type 'UnsafePointer<Int>' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(ap) // expected-error{{cannot convert value of type 'AutoreleasingUnsafeMutablePointer<Int>' to expected argument type 'UnsafeMutablePointer<Int>'}}
  var i: Int = 0
  var f: Float = 0
  takesMutablePointer(&i)
  takesMutablePointer(&f) // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(f) // expected-error{{cannot convert value of type 'Float' to expected argument type 'UnsafeMutablePointer<Int>'}}
  var ii: [Int] = [0, 1, 2]
  var ff: [Float] = [0, 1, 2]
  takesMutablePointer(&ii)
  takesMutablePointer(&ff) // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(ii) // expected-error{{cannot convert value of type '[Int]' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'UnsafeMutablePointer<Int>'}}

  takesMutableArrayPointer(&i) // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<[Int]>' (aka 'UnsafeMutablePointer<Array<Int>>')}}
  takesMutableArrayPointer(&ii)

  // We don't allow these conversions outside of function arguments.
  var x: UnsafeMutablePointer<Int> = &i // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int>'}}
  x = &ii // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int>'}}
  _ = x
}

func mutableVoidPointerArguments(p: UnsafeMutablePointer<Int>,
                                 cp: UnsafePointer<Int>,
                                 ap: AutoreleasingUnsafeMutablePointer<Int>,
                                 fp: UnsafeMutablePointer<Float>) {
  takesMutableVoidPointer(nil)
  takesMutableVoidPointer(p)
  takesMutableVoidPointer(fp)
  takesMutableVoidPointer(cp) // expected-error{{cannot convert value of type 'UnsafePointer<Int>' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  takesMutableVoidPointer(ap) // expected-error{{cannot convert value of type 'AutoreleasingUnsafeMutablePointer<Int>' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  var i: Int = 0
  var f: Float = 0
  takesMutableVoidPointer(&i)
  takesMutableVoidPointer(&f)
  takesMutableVoidPointer(i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  takesMutableVoidPointer(f) // expected-error{{cannot convert value of type 'Float' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  var ii: [Int] = [0, 1, 2]
  var dd: [CInt] = [1, 2, 3]
  var ff: [Int] = [0, 1, 2]
  takesMutableVoidPointer(&ii)
  takesMutableVoidPointer(&dd)
  takesMutableVoidPointer(&ff)
  takesMutableVoidPointer(ii) // expected-error{{cannot convert value of type '[Int]' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  takesMutableVoidPointer(ff) // expected-error{{cannot convert value of type '[Int]' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}

  // We don't allow these conversions outside of function arguments.
  var x: UnsafeMutablePointer<Void> = &i // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  x = p // expected-error{{cannot assign value of type 'UnsafeMutablePointer<Int>' to type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  x = &ii // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  _ = x
}

func constPointerArguments(p: UnsafeMutablePointer<Int>,
                           cp: UnsafePointer<Int>,
                           ap: AutoreleasingUnsafeMutablePointer<Int>) {
  takesConstPointer(nil)
  takesConstPointer(p)
  takesConstPointer(cp)
  takesConstPointer(ap)

  var i: Int = 0
  var f: Float = 0
  takesConstPointer(&i)
  takesConstPointer(&f) // expected-error{{'&' used with non-inout argument of type 'UnsafePointer<Int>'}}
  var ii: [Int] = [0, 1, 2]
  var ff: [Float] = [0, 1, 2]
  takesConstPointer(&ii)
  takesConstPointer(&ff) // expected-error{{'&' used with non-inout argument of type 'UnsafePointer<Int>'}}
  takesConstPointer(ii)
  takesConstPointer(ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'UnsafePointer<Int>'}}
  takesConstPointer([0, 1, 2])
  // <rdar://problem/22308330> QoI: CSDiags doesn't handle array -> pointer impl conversions well
  takesConstPointer([0.0, 1.0, 2.0]) // expected-error{{cannot convert value of type 'Double' to expected element type 'Int'}}

  // We don't allow these conversions outside of function arguments.
  var x: UnsafePointer<Int> = &i // expected-error{{'&' used with non-inout argument of type 'UnsafePointer<Int>'}}
  x = ii // expected-error{{cannot assign value of type '[Int]' to type 'UnsafePointer<Int>'}}
  x = p // expected-error{{cannot assign value of type 'UnsafeMutablePointer<Int>' to type 'UnsafePointer<Int>'}}
  x = ap // expected-error{{cannot assign value of type 'AutoreleasingUnsafeMutablePointer<Int>' to type 'UnsafePointer<Int>'}}
}

func constVoidPointerArguments(p: UnsafeMutablePointer<Int>,
                               fp: UnsafeMutablePointer<Float>,
                               cp: UnsafePointer<Int>,
                               cfp: UnsafePointer<Float>,
                               ap: AutoreleasingUnsafeMutablePointer<Int>,
                               afp: AutoreleasingUnsafeMutablePointer<Float>) {
  takesConstVoidPointer(nil)
  takesConstVoidPointer(p)
  takesConstVoidPointer(fp)
  takesConstVoidPointer(cp)
  takesConstVoidPointer(cfp)
  takesConstVoidPointer(ap)
  takesConstVoidPointer(afp)

  var i: Int = 0
  var f: Float = 0
  takesConstVoidPointer(&i)
  takesConstVoidPointer(&f)
  var ii: [Int] = [0, 1, 2]
  var ff: [Float] = [0, 1, 2]
  takesConstVoidPointer(&ii)
  takesConstVoidPointer(&ff)
  takesConstVoidPointer(ii)
  takesConstVoidPointer(ff)
  takesConstVoidPointer([0, 1, 2]) // expected-error {{cannot convert value of type 'Int' to expected element type '()'}}
takesConstVoidPointer([0.0, 1.0, 2.0])  // expected-error {{cannot convert value of type 'Double' to expected element type '()'}}

  // We don't allow these conversions outside of function arguments.
  var x: UnsafePointer<Void> = &i // expected-error{{'&' used with non-inout argument of type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = ii // expected-error{{cannot assign value of type '[Int]' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = p // expected-error{{cannot assign value of type 'UnsafeMutablePointer<Int>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = fp // expected-error{{cannot assign value of type 'UnsafeMutablePointer<Float>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = cp // expected-error{{cannot assign value of type 'UnsafePointer<Int>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = cfp // expected-error{{cannot assign value of type 'UnsafePointer<Float>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = ap // expected-error{{cannot assign value of type 'AutoreleasingUnsafeMutablePointer<Int>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  x = afp // expected-error{{cannot assign value of type 'AutoreleasingUnsafeMutablePointer<Float>' to type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
  _ = x
}

func stringArguments(s: String) {
  var s = s
  takesConstVoidPointer(s)
  takesConstInt8Pointer(s)
  takesConstUInt8Pointer(s)
  takesConstPointer(s) // expected-error{{cannot convert value of type 'String' to expected argument type 'UnsafePointer<Int>'}}

  takesMutableVoidPointer(s) // expected-error{{cannot convert value of type 'String' to expected argument type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  takesMutableInt8Pointer(s) // expected-error{{cannot convert value of type 'String' to expected argument type 'UnsafeMutablePointer<Int8>'}}
  takesMutableInt8Pointer(&s) // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int8>'}}
  takesMutablePointer(s) // expected-error{{cannot convert value of type 'String' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(&s) // expected-error{{'&' used with non-inout argument of type 'UnsafeMutablePointer<Int>'}}
}

func autoreleasingPointerArguments(p: UnsafeMutablePointer<Int>,
                                   cp: UnsafePointer<Int>,
                                   ap: AutoreleasingUnsafeMutablePointer<C>) {
  takesAutoreleasingPointer(nil)
  takesAutoreleasingPointer(p) // expected-error{{cannot convert value of type 'UnsafeMutablePointer<Int>' to expected argument type 'AutoreleasingUnsafeMutablePointer<C>'}}
  takesAutoreleasingPointer(cp) // expected-error{{cannot convert value of type 'UnsafePointer<Int>' to expected argument type 'AutoreleasingUnsafeMutablePointer<C>'}}
  takesAutoreleasingPointer(ap)

  var c: C = C()
  takesAutoreleasingPointer(&c)
  takesAutoreleasingPointer(c) // expected-error{{cannot convert value of type 'C' to expected argument type 'AutoreleasingUnsafeMutablePointer<C>'}}
  var d: D = D()
  takesAutoreleasingPointer(&d) // expected-error{{'&' used with non-inout argument of type 'AutoreleasingUnsafeMutablePointer<C>'}}
  takesAutoreleasingPointer(d) // expected-error{{cannot convert value of type 'D' to expected argument type 'AutoreleasingUnsafeMutablePointer<C>'}}
  var cc: [C] = [C(), C()]
  var dd: [D] = [D(), D()]
  takesAutoreleasingPointer(&cc) // expected-error{{'&' used with non-inout argument of type 'AutoreleasingUnsafeMutablePointer<C>'}}
  takesAutoreleasingPointer(&dd) // expected-error{{'&' used with non-inout argument of type 'AutoreleasingUnsafeMutablePointer<C>'}}

  let _: AutoreleasingUnsafeMutablePointer<C> = &c // expected-error{{'&' used with non-inout argument of type 'AutoreleasingUnsafeMutablePointer<C>'}}
}

func pointerConstructor(x: UnsafeMutablePointer<Int>) -> UnsafeMutablePointer<Float> {
  return UnsafeMutablePointer(x)
}

func pointerArithmetic(x: UnsafeMutablePointer<Int>, y: UnsafeMutablePointer<Int>,
                       i: Int) {
  _ = x + i
  _ = x - y
}

func genericPointerArithmetic<T>(x: UnsafeMutablePointer<T>, i: Int, t: T) -> UnsafeMutablePointer<T> {
  let p = x + i
  p.initialize(t)
}

func passPointerToClosure(f: UnsafeMutablePointer<Float> -> Int) -> Int { }

func pointerInClosure(f: UnsafeMutablePointer<Int> -> Int) -> Int {
  return passPointerToClosure { f(UnsafeMutablePointer($0)) }
}

struct NotEquatable {}

func arrayComparison(x: [NotEquatable], y: [NotEquatable], p: UnsafeMutablePointer<NotEquatable>) {
  var x = x
  // Don't allow implicit array-to-pointer conversions in operators.
  let a: Bool = x == y // expected-error{{binary operator '==' cannot be applied to two '[NotEquatable]' operands}}

  let _: Bool = p == &x  // Allowed!
}

func addressConversion(p: UnsafeMutablePointer<Int>, x: Int) {
  var x = x
  let _: Bool = p == &x
}
