//===----------------------------------------------------------------------===//
// LogicValue
//===----------------------------------------------------------------------===//

@prefix func !<T : LogicValue>(a: T) -> Bool {
  return !a.getLogicValue()
}

// Short circuiting logical operators.

// FIXME: these operators should be fully generic
// BLOCKED ON: <rdar://problem/13251236> [remapping bound function type not
// implemented yet (deduced closure types)].
//
// FIXME: the generic versions of these operators probably shouldn't
// be @transparent; ideally they will be overloaded with transparent
// bool-specific operators.  BLOCKED ON: <rdar://problem/11510876>
// [Implement overload resolution].

@transparent
func &&(lhs: LogicValue, rhs: @auto_closure () -> LogicValue) -> Bool {
  return lhs.getLogicValue() ? rhs().getLogicValue() : false
}

@transparent
func ||(lhs: LogicValue, rhs: @auto_closure () -> LogicValue) -> Bool {
  return lhs.getLogicValue() ? true : rhs().getLogicValue()
}
