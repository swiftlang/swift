/// \brief A wrapper around Builtin.ObjectPointer that encapsulates
/// "higher-level" features like Null checking, comparison, etc.  Like
/// Builtin.XXXX, this component is not intended for user-consumption;
/// it is just for the convenience of stdlib implementers.
struct _ObjectPointer : LogicValue, Equatable {
  typealias Me = _ObjectPointer
  typealias Base = Builtin.ObjectPointer
  
  init() {
    base = Builtin.bridgeFromRawPointer(Builtin.inttoptr_Int64(0.value))
  }

  @conversion
  def __conversion() -> Base {
    return base
  }
  
  static def Null() -> Me {
    return Me()
  }

  def __equal__(rhs: Me) -> Bool {
    return self.bits == rhs.bits
  }
  
  def getLogicValue() -> Bool {
    return bits == 0
  }

  var bits : Int64 {
    return Int64(Builtin.ptrtoint_Int64(Builtin.bridgeToRawPointer(base)))
  }
  
  var base: Base
}
