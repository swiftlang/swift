// RUN: %swift -I %S/.. %s -i | FileCheck %s

// Define a Complex number.
struct Complex {
  Real : Double,
  Imaginary : Double
  static func zero() -> Complex {
    return Complex(0.0, 0.0)
  }
  func magnitude() -> Double {
    return Real * Real + Imaginary * Imaginary
  }
}

func [infix_left=200] * (lhs : Complex, rhs : Complex) -> Complex {
  return Complex(lhs.Real * rhs.Real - lhs.Imaginary * rhs.Imaginary,
                 lhs.Real * rhs.Imaginary + lhs.Imaginary * rhs.Real)
}
func [infix_left=190] + (lhs: Complex, rhs: Complex) -> Complex {
  return Complex(lhs.Real + rhs.Real, lhs.Imaginary + rhs.Imaginary)
}

// FIXME: Could be made a nested function of 'mandelbrot' once nested
// functions are working.
func inMandelbrotSet(z:Complex, c:Complex, n:Int) -> Bool {
  while (n > 0) {
    if (z.magnitude() > 4.0) {
      return Bool.false;
    }
    z = z*z + c
    n = n - 1
  }
  return Bool.true
}

func mandelbrot(xMin:Double, xMax:Double,
                yMin:Double, yMax:Double,
                rows:Int, cols:Int,
                maxIterations:Int)  {
  // Set the spacing for the poInts in the Mandelbrot set.
  var dX = (xMax - xMin) / Double(rows)
  var dY = (yMax - yMin) / Double(cols)
  // Iterate over the poInts an determine if they are in the
  // Mandelbrot set.
  var row = 0
  while (row < rows) {
    row = row + 1
    var col = 0;
    while (col < cols) {
      col = col + 1
      var c = Complex(xMin + (dX * Double(row)),
                      yMin + (dY * Double(col)))
      var out = " "
      if (inMandelbrotSet(Complex.zero(), c, maxIterations)) {
        out = "*"
      }
      print(out)
    }
    print("\n")
  }
}

mandelbrot(-1.5, 0.5, -1.0, 1.0, 50, 80, 200)

// CHECK:                                    *
// CHECK:                                    *
// CHECK:                                    *
// CHECK:                                  *****
// CHECK:                                  *****
// CHECK:                                   ***
// CHECK:                              *************
// CHECK:                             ***************
// CHECK:                          *********************
// CHECK:                           *******************
// CHECK:                          *********************
// CHECK:                          *********************
// CHECK:                          *********************
// CHECK:                           *******************
// CHECK:                           *******************
// CHECK:                            *****************
// CHECK:                              *************
// CHECK:                                *********
// CHECK:                              *************
// CHECK:                         ***********************
// CHECK:                       ***************************
// CHECK:                  ** ******************************* **
// CHECK:                  *************************************
// CHECK:            ***  ***************************************  ***
// CHECK:            *************************************************
// CHECK:               *******************************************
// CHECK:             ***********************************************
// CHECK:            *************************************************
// CHECK:            *************************************************
// CHECK:            *************************************************
// CHECK:           ***************************************************
// CHECK:    * *    ***************************************************    * *
// CHECK:    ****** *************************************************** ******
// CHECK:   ******* *************************************************** *******
// CHECK: ********* *************************************************** *********
// CHECK:    *****************************************************************
// CHECK:           ***************************************************
// CHECK:           ***************************************************
// CHECK:           ***************************************************
// CHECK:            *************************************************
// CHECK:             ***********************************************
// CHECK:              *********************************************
// CHECK:               *******************************************
// CHECK:              ********************** **********************
// CHECK:              ***  ****************   ****************  ***
// CHECK:                     *************     *************
// CHECK:                      **   *  *           *  *   **

