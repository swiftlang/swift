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
  takesMutablePointer(&f) // expected-error{{cannot convert value of type 'Float' to expected argument type 'Int'}}
  takesMutablePointer(i) // expected-error{{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(f) // expected-error{{cannot convert value of type 'Float' to expected argument type 'UnsafeMutablePointer<Int>'}}
  var ii: [Int] = [0, 1, 2]
  var ff: [Float] = [0, 1, 2]
  takesMutablePointer(&ii)
  takesMutablePointer(&ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'Int'}}
  takesMutablePointer(ii) // expected-error{{cannot convert value of type '[Int]' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'UnsafeMutablePointer<Int>'}}

  takesMutableArrayPointer(&i) // expected-error{{cannot convert value of type 'Int' to expected argument type '[Int]'}}
  takesMutableArrayPointer(&ii)

  // We don't allow these conversions outside of function arguments.
  var x: UnsafeMutablePointer<Int> = &i // expected-error{{cannot pass immutable value of type 'Int' as inout argument}}
  x = &ii // expected-error{{cannot assign value of type '[Int]' to type 'Int'}}
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
  var x: UnsafeMutablePointer<Void> = &i // expected-error{{cannot convert value of type 'inout Int' to specified type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  x = p // expected-error{{cannot assign value of type 'UnsafeMutablePointer<Int>' to type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
  x = &ii // expected-error{{cannot assign value of type 'inout [Int]' (aka 'inout Array<Int>') to type 'UnsafeMutablePointer<Void>' (aka 'UnsafeMutablePointer<()>')}}
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
  takesConstPointer(&f) // expected-error{{cannot convert value of type 'Float' to expected argument type 'Int'}}
  var ii: [Int] = [0, 1, 2]
  var ff: [Float] = [0, 1, 2]
  takesConstPointer(&ii)
  takesConstPointer(&ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'Int'}}
  takesConstPointer(ii)
  takesConstPointer(ff) // expected-error{{cannot convert value of type '[Float]' to expected argument type 'UnsafePointer<Int>'}}
  takesConstPointer([0, 1, 2])
  // <rdar://problem/22308330> QoI: CSDiags doesn't handle array -> pointer impl conversions well
  takesConstPointer([0.0, 1.0, 2.0]) // expected-error{{cannot convert value of type 'Double' to expected element type 'Int'}}

  // We don't allow these conversions outside of function arguments.
  var x: UnsafePointer<Int> = &i // expected-error{{cannot pass immutable value of type 'Int' as inout argument}}
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
                                
  // TODO: These two should be accepted, tracked by rdar://17444930.
  takesConstVoidPointer([0, 1, 2]) // expected-error {{cannot convert value of type 'Int' to expected element type '()'}}
takesConstVoidPointer([0.0, 1.0, 2.0])  // expected-error {{cannot convert value of type 'Double' to expected element type '()'}}

  // We don't allow these conversions outside of function arguments.
  var x: UnsafePointer<Void> = &i // expected-error{{cannot convert value of type 'inout Int' to specified type 'UnsafePointer<Void>' (aka 'UnsafePointer<()>')}}
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
  takesMutableInt8Pointer(&s) // expected-error{{cannot convert value of type 'String' to expected argument type 'Int8'}}
  takesMutablePointer(s) // expected-error{{cannot convert value of type 'String' to expected argument type 'UnsafeMutablePointer<Int>'}}
  takesMutablePointer(&s) // expected-error{{cannot convert value of type 'String' to expected argument type 'Int'}}
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
  takesAutoreleasingPointer(&d) // expected-error{{cannot convert value of type 'D' to expected argument type 'C'}}
  takesAutoreleasingPointer(d) // expected-error{{cannot convert value of type 'D' to expected argument type 'AutoreleasingUnsafeMutablePointer<C>'}}
  var cc: [C] = [C(), C()]
  var dd: [D] = [D(), D()]
  takesAutoreleasingPointer(&cc) // expected-error{{cannot convert value of type '[C]' to expected argument type 'C'}}
  takesAutoreleasingPointer(&dd) // expected-error{{cannot convert value of type '[D]' to expected argument type 'C'}}

  let _: AutoreleasingUnsafeMutablePointer<C> = &c // expected-error{{cannot pass immutable value of type 'C' as inout argument}}
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
  p.initializeMemory(t)
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
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists:}}

  let _: Bool = p == &x  // Allowed!
}

func addressConversion(p: UnsafeMutablePointer<Int>, x: Int) {
  var x = x
  let _: Bool = p == &x
}

// <rdar://problem/19478919> QoI: poor error: '&' used with non-inout argument of type 'UnsafeMutablePointer<Int32>'
func f19478919() {
  var viewport: Int = 1 // intentionally incorrect type, not Int32
  func GLKProject(a : UnsafeMutablePointer<Int32>) {}
  GLKProject(&viewport)  // expected-error {{cannot convert value of type 'Int' to expected argument type 'Int32'}}

  func GLKProjectUP(a : UnsafePointer<Int32>) {}
  func UP_Void(a : UnsafePointer<Void>) {}
  func UMP_Void(a : UnsafeMutablePointer<Void>) {}
  UP_Void(&viewport)
  UMP_Void(&viewport)

  let cst = 42  // expected-note 2 {{change 'let' to 'var' to make it mutable}}
  UP_Void(&cst)  // expected-error {{cannot pass immutable value as inout argument: 'cst' is a 'let' constant}}
  UMP_Void(&cst)  // expected-error {{cannot pass immutable value as inout argument: 'cst' is a 'let' constant}}
}

// <rdar://problem/23202128> QoI: Poor diagnostic with let vs. var passed to C function
func f23202128() {
  func UP(p: UnsafePointer<Int32>) {}
  func UMP(p: UnsafeMutablePointer<Int32>) {}

  let pipe: [Int32] = [0, 0]  // expected-note {{change 'let' to 'var' to make it mutable}}}}
  UMP(&pipe)  // expected-error {{cannot pass immutable value as inout argument: 'pipe' is a 'let' constant}}

  var pipe2: [Int] = [0, 0]
  UMP(&pipe2) // expected-error {{cannot convert value of type '[Int]' to expected argument type 'Int32'}}

  
  UP(pipe)    // ok
  UP(&pipe)   // expected-error {{'&' is not allowed passing array value as 'UnsafePointer<Int32>' argument}} {{6-7=}}
}



