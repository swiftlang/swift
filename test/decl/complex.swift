// RUN: %swift -verify -parse %s

// Work in progress for the April demo.  Note the various implementations that
// are commented out, these correspond to the progression in the slides.

/*
// Simple and good, too simple for its own good.
struct Complex { 
  R : Double,
  I : Double
}*/

/*

// This doesn't build, it is what you'd have to do if you don't have properties.
// That and Search and replace.
struct Complex { 
  Radius : Double,
  Angle : Double
  
  def getReal() -> Double { return Radius*cos(Angle) }
  def getImag() -> Double { return Radius*sin(Angle) }
  
  def setReal(X: Double) { setRealImag(X, getImag()) }
  def setImag(X: Double) { setRealImag(getReal(), X) }
  
  def setRealImag(Real: Double, Imag: Double) {
    Angle = atan(Imag, Real)
    Radius = sqrt(Real*Real + Imag*Imag)
  }
}*/

// Implementation that uses properties to remap direct uses of .R and .I onto
// Radius/Angle implementations.
struct Complex { 
  var Radius : Double,
  Angle : Double
  
  var R : Double {
     get: return Radius*cos(Angle)
     set(val): setRealImag(val, Radius*sin(Angle))
  }
  var I : Double {
    get: return Radius*sin(Angle)
    set(val): setRealImag(Radius*cos(Angle), val)
  }
  
  def setRealImag(Real: Double, Imag: Double) {
    Angle = atan(Imag, Real)
    Radius = sqrt(Real*Real + Imag*Imag)
  }
}


// Some "legacy" code using .I and .R.
extension Complex {
  def magnitude() -> Double {
    return sqrt(R*R + I*I)
  }
}

extension Complex {
  def replPrint() {
    print("(\(R), \(I))")
  }
}

extension Complex {
  def add(V: Complex) -> Complex {
    return Complex(R+V.R, I+V.I)
  }
  def mul(V: Complex) -> Complex {
    return Complex(R*V.R - I*V.I, R*V.I + I*V.R)
  }
}

def +(lhs: Complex, rhs: Complex) -> Complex {
  return Complex(lhs.R+rhs.R, lhs.I+rhs.I)
}

def *(lhs: Complex, rhs: Complex) -> Complex {
  return Complex(lhs.R*rhs.R - lhs.I*rhs.I, lhs.R*rhs.I + lhs.I*rhs.R)
}


extension String {
  init(v: Complex, polar: Bool = false) {
    if (!polar) {
      self = "(\(v.R), \(v.I))"
    } else {
      self = "(\(v.Radius), \(v.Angle))"
    }
  }
}

