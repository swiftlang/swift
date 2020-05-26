// RUN: not %target-swift-frontend -typecheck %s
enum S<Value> {
  @propertyWrapper 
  private struct A {
    var s:UInt = 0
    var wrappedValue:Value { didSet { } }
  
    init(wrappedValue:Value) { self.wrappedValue = wrappedValue }
  }
    
  @propertyWrapper 
  final class B {
    @A 
    var wrappedValue:Value 
    
    var projectedValue:S<Value>.B  
    {
      self
    }
    
    init(wrappedValue:Value) 
    {
      self.wrappedValue = wrappedValue
    }
  }
    
  @propertyWrapper 
  struct O {
    private  var s:UInt? = nil 
    @B private(set) var wrappedValue:Value 
    
    var a:Bool 
    {
      self.s.map{ $0 != self.$wrappedValue.wrappedValue.sequence } ?? true
    }
  }
}
