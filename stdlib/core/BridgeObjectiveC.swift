/// \brief A means of accepting "out" parameters from Objective-C
/// functions taking the parameters by pointer
func withOutArgument<T, Result>(
  arg: @inout T,
  f: (UnsafePointer<T>)->Result
) -> Result
{
  return f(UnsafePointer<T>(Builtin.addressof(&arg)))
}

/// \brief A means of accepting @autorelease "out" parameters from
/// Objective-C functions, which can be tricky because they are
/// not handed to us at +1 like other objects.
func withAutoReleasedOutArgument<T, Result>(
  arg: @inout T?,
  f: (UnsafePointer<T>)->Result
) -> Result
{
  var buffer: Builtin.RawPointer = Builtin.inttoptr_Int64(0.value)
  var address = UnsafePointer<T>(Builtin.addressof(&buffer))
  var result = f(address)
  
  arg = Int64(Builtin.ptrtoint_Int64(buffer)) == 0
    ? .None : .Some(address.get())

  return result
}
