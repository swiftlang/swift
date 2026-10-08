@inline(never)
public func erase<T: IItem>(_ value: T) -> any IItem {
  value
}

@inline(never)
public func eraseConsumed<T: IItem>(_ value: consuming T) -> any IItem {
  consume value
}

@inline(never)
public func inherited<T: IExtended>(_ value: T) -> any IItem {
  value
}

@inline(never)
public func eraseClass<T: IClassItem>(_ value: T) -> any IClassItem {
  value
}

@inline(never)
public func captureErasure<T: IItem>(_ value: T) -> () -> any IItem {
  { value }
}

@inline(never)
public func erasePack<each T: IItem>(_ values: repeat each T) -> [any IItem] {
  var result: [any IItem] = []
  for value in repeat each values { result.append(value) }
  return result
}

@inlinable
public func inlineErase<T: IItem>(_ value: T) -> any IItem {
  value
}

@inline(never)
public func refine(_ value: consuming any IExtended) -> any IItem {
  value
}

@inline(never)
public func copyBorrowed<T: IItem>(_ value: borrowing T) -> any IItem {
  copy value
}

@inline(never)
public func eraseBorrowed<T: IItem>(_ value: borrowing T) -> any IItem {
  value
}
