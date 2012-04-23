// RUN: %swift %s -parse -verify

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
  
  func getReal() -> Double { return Radius*cos(Angle) }
  func getImag() -> Double { return Radius*sin(Angle) }
  
  func setReal(X : Double) { setRealImag(X, getImag()) }
  func setImag(X : Double) { setRealImag(getReal(), X) }
  
  func setRealImag(Real : Double, Imag : Double) {
    Angle = atan2(Imag, Real)
    Radius = sqrt(Real*Real + Imag*Imag)
  }
}*/

// Implementation that uses properties to remap direct uses of .R and .I onto
// Radius/Angle implementations.
struct Complex { 
  Radius : Double,
  Angle : Double
  
  var R : Double {
     get { return Radius*cos(Angle) }
     set(val) { setRealImag(val, Radius*sin(Angle)) }
  }
  var I : Double {
    get { return Radius*sin(Angle) } 
    set(val) { setRealImag(Radius*cos(Angle), val) }
  }
  
  func setRealImag(Real : Double, Imag : Double) {
    Angle = atan2(Imag, Real)
    Radius = sqrt(Real*Real + Imag*Imag)
  }
}


// Some "legacy" code using .I and .R.
extension Complex {
   func magnitude() -> Double {
      return sqrt(R*R + I*I)
   }
}

extension Complex {
  func replPrint() {
     print("(" + String(R) + ", " + String(I) + ")")
  }
}

extension Complex {
  func add(V : Complex) -> Complex {
    return Complex(R+V.R, I+V.I)
  }
  func mul(V : Complex) -> Complex {
    return Complex(R*V.R - I*V.I, R*V.I + I*V.R)
  }
}


