// RUN: %target-typecheck-verify-swift -solver-scope-threshold=500

// https://github.com/swiftlang/swift/issues/48155

public struct MatrixNxM<T> {
    var m: [T]
    private let xdim : Int
    private let ydim : Int
    
    subscript(i: Int, j: Int) -> T {
        get {
            precondition(i*xdim + j < self.m.count, "Index out of matrix bounds \(i) \(j)")
            
            return self.m[i*xdim + j]
        }
        set {
            self.m[i*xdim + j] = newValue
        }
    }
    
    public init(xdim: Int, ydim: Int) {
        self.xdim = xdim
        self.ydim = ydim
        self.m = [Any](repeating: 0.0, count: xdim*ydim) as! [T]
    }
}

public struct Matrix4x4 {
    var m: [Double]
    
    subscript(i: Int, j: Int) -> Double {
        get {
            precondition(i*4 + j < self.m.count, "Index out of matrix bounds \(i) \(j)")
            
            return self.m[i*4 + j]
        }
        set {
            self.m[i*4 + j] = newValue
        }
    }
    
    public init() {
        m = [Double](repeating: 0.0, count: 16)
    }
    
    public static func identity() -> Matrix4x4 {
        var identity = Matrix4x4()
    
        identity[0,0] = 1.0;
        identity[1,1] = 1.0;
        identity[2,2] = 1.0;
        identity[3,3] = 1.0;
    
        return identity
    }
    
    public func determinant() -> Double {
        let det = self[0,0] * self[1,1] * self[2,2] * self[3,3] +
            self[0,0] * self[1,2] * self[2,3] * self[3,1] +
            self[0,0] * self[1,3] * self[2,1] * self[3,2] +
            
            self[0,1] * self[1,0] * self[2,3] * self[3,2] +
            self[0,1] * self[1,2] * self[2,1] * self[3,3] +
            self[0,1] * self[1,3] * self[2,2] * self[3,0] +
            
            self[0,2] * self[1,0] * self[2,1] * self[3,3] +
            self[0,2] * self[1,1] * self[2,3] * self[3,0] +
            self[0,2] * self[1,3] * self[2,0] * self[3,0] +
            
            self[0,3] * self[1,0] * self[2,2] * self[3,1] +
            self[0,3] * self[1,1] * self[2,0] * self[3,2] +
            self[0,3] * self[1,2] * self[2,1] * self[3,0] -
            
            self[0,0] * self[1,1] * self[2,3] * self[3,2] -
            self[0,0] * self[1,2] * self[2,1] * self[3,3] -
            self[0,0] * self[1,3] * self[2,2] * self[3,1] -
            
            self[0,1] * self[1,0] * self[2,2] * self[3,3] -
            self[0,1] * self[1,2] * self[2,3] * self[3,0] -
            self[0,1] * self[1,3] * self[2,0] * self[3,2] -
            
            self[0,2] * self[1,0] * self[2,3] * self[3,1] -
            self[0,2] * self[1,1] * self[2,0] * self[3,3] -
            self[0,2] * self[1,3] * self[2,1] * self[3,0] -
            
            self[0,3] * self[1,0] * self[2,1] * self[3,2] -
            self[0,3] * self[1,1] * self[2,2] * self[3,0] -
            self[0,3] * self[1,2] * self[2,0] * self[3,1]
        
        return det
    }
    
    public func inverse() -> Matrix4x4 {
        let det = self.determinant()
        precondition(det != 0, "Determinant is zero, unable to calculate inverse Matrix")
        
        let inv_det = 1.0 / det
        var inverse = Matrix4x4()
        
        inverse[0, 0] = (self[1,1] * self[2,2] * self[3,3] +
            self[1,2] * self[2,3] * self[3,1] +
            self[1,3] * self[2,1] * self[3,2] -
            self[1,1] * self[2,3] * self[3,2] -
            self[1,2] * self[2,1] * self[3,3] -
            self[1,3] * self[2,2] * self[3,1]) * inv_det;
        
        inverse[0, 1] = (self[0,1] * self[2,3] * self[3,2] +
            self[0,2] * self[2,1] * self[3,3] +
            self[1,3] * self[2,2] * self[3,1] -
            self[0,1] * self[2,2] * self[3,3] -
            self[0,2] * self[2,3] * self[3,1] -
            self[0,3] * self[2,1] * self[3,2]) * inv_det;
        
        inverse[0, 2] = (self[0,1] * self[1,2] * self[3,3] +
            self[0,2] * self[1,3] * self[3,1] +
            self[0,3] * self[1,1] * self[3,2] -
            self[0,1] * self[1,3] * self[3,2] -
            self[0,2] * self[1,1] * self[3,3] -
            self[0,3] * self[1,2] * self[3,1]) * inv_det;
        
        inverse[0, 3] = (self[0,1] * self[1,3] * self[2,2] +
            self[0,2] * self[1,1] * self[2,3] +
            self[0,3] * self[1,2] * self[2,1] -
            self[0,1] * self[1,2] * self[2,3] -
            self[0,2] * self[1,3] * self[2,1] -
            self[0,3] * self[1,1] * self[2,2]) * inv_det;
        
        inverse[1, 0] = (self[1,0] * self[2,3] * self[3,2] +
            self[1,2] * self[2,0] * self[3,3] +
            self[1,3] * self[2,2] * self[3,1] -
            self[1,0] * self[2,2] * self[3,3] -
            self[1,2] * self[2,3] * self[3,1] -
            self[1,3] * self[2,0] * self[3,2]) * inv_det;
        
        inverse[1, 1] = (self[0,0] * self[2,2] * self[3,3] +
            self[0,2] * self[2,3] * self[3,1] +
            self[0,3] * self[2,0] * self[3,2] -
            self[0,0] * self[2,3] * self[3,2] -
            self[0,2] * self[2,0] * self[3,3] -
            self[0,3] * self[2,2] * self[3,0]) * inv_det;
        
        
        inverse[1, 2] = (self[0,0] * self[1,3] * self[3,2] +
            self[0,2] * self[1,0] * self[3,3] +
            self[0,3] * self[1,2] * self[3,1] -
            self[0,0] * self[1,2] * self[3,3] -
            self[0,2] * self[1,3] * self[3,0] -
            self[0,3] * self[1,0] * self[3,2]) * inv_det;
        
        inverse[1, 3] = (self[0,0] * self[1,2] * self[2,3] +
            self[0,2] * self[1,3] * self[2,2] +
            self[0,3] * self[1,0] * self[2,2] -
            self[0,0] * self[1,3] * self[2,2] -
            self[0,2] * self[1,0] * self[2,3] -
            self[0,3] * self[1,2] * self[2,0]) * inv_det;
        
        inverse[2, 0] = (self[1,0] * self[2,1] * self[3,3] +
            self[1,1] * self[2,3] * self[3,1] +
            self[1,3] * self[2,1] * self[3,2] -
            self[1,0] * self[2,3] * self[3,1] -
            self[1,1] * self[2,0] * self[3,3] -
            self[1,3] * self[2,1] * self[3,0]) * inv_det;
        
        inverse[2, 1] = (self[0,0] * self[2,3] * self[3,1] +
            self[0,1] * self[2,0] * self[3,3] +
            self[0,3] * self[2,1] * self[3,1] -
            self[0,0] * self[2,1] * self[3,3] -
            self[0,1] * self[2,3] * self[3,0] -
            self[0,3] * self[2,0] * self[3,1]) * inv_det;
        
        inverse[2, 2] = (self[0,0] * self[1,1] * self[3,3] +
            self[0,1] * self[1,3] * self[3,0] +
            self[0,3] * self[1,0] * self[3,1] -
            self[0,0] * self[1,3] * self[3,1] -
            self[0,1] * self[1,0] * self[3,3] -
            self[0,3] * self[1,1] * self[3,0]) * inv_det;
        
        inverse[2, 3] = (self[0,0] * self[1,3] * self[2,1] +
            self[0,1] * self[1,0] * self[2,3] +
            self[0,3] * self[1,1] * self[2,0] -
            self[0,0] * self[1,1] * self[2,3] -
            self[0,1] * self[1,3] * self[2,0] -
            self[0,3] * self[1,0] * self[2,1]) * inv_det;
        
        inverse[3, 0] = (self[1,0] * self[2,2] * self[3,1] +
            self[1,1] * self[2,0] * self[3,2] +
            self[1,2] * self[2,1] * self[3,0] -
            self[1,0] * self[2,1] * self[3,2] -
            self[1,1] * self[2,2] * self[3,0] -
            self[1,2] * self[2,0] * self[3,1]) * inv_det;
        
        inverse[3, 1] = (self[0,0] * self[2,1] * self[3,2] +
            self[0,1] * self[2,2] * self[3,0] +
            self[0,2] * self[2,0] * self[3,2] -
            self[0,0] * self[2,2] * self[3,1] -
            self[0,1] * self[2,0] * self[3,2] -
            self[0,2] * self[2,1] * self[3,0]) * inv_det;
        
        inverse[3, 2] = (self[0,0] * self[1,2] * self[3,1] +
            self[0,1] * self[1,0] * self[3,2] +
            self[0,2] * self[1,1] * self[3,0] -
            self[0,0] * self[1,1] * self[3,2] -
            self[0,1] * self[1,2] * self[3,0] -
            self[0,2] * self[1,0] * self[3,1]) * inv_det;
        
        inverse[3, 3] = (self[0,0] * self[1,1] * self[2,2] +
            self[0,1] * self[1,2] * self[2,0] +
            self[0,2] * self[2,1] * self[2,1] -
            self[0,0] * self[1,2] * self[2,1] -
            self[0,1] * self[1,0] * self[2,2] -
            self[0,2] * self[1,1] * self[2,0]) * inv_det;
        
        return inverse
    }
}

public func * (a: Matrix4x4, b: Matrix4x4) -> Matrix4x4 {
    var c = Matrix4x4();
    
    for i in 0..<4 {
        for j in 0..<4 {
            var ab: Double = 0.0;
            for k in 0..<4 {
                ab += a[i,k] * b[k,j];
            }
            
            c[i, j] = ab;
        }
    }
    
    return c
}
