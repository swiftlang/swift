// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> Self {
        return b(self.dynamicType) 
    }
}
func b<T>(t: AnyObject.Type) -> T! {
    return nil
}
