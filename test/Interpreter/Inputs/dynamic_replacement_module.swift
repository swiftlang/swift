#if MODULE
public dynamic func public_global_func() -> String {
  return "public_global_func"
}

public dynamic func public_global_generic_func<T>(_ t: T.Type) -> String {
  return "public_global_generic_func"
}

public class PublicClass {
  public var str : String = ""
  public init() {}
  public dynamic init(x: Int) { str = "public_class_init" }

  public dynamic func function() -> String {
    return "public_class_func"
  }
  public dynamic func genericFunction<T>(_ t: T.Type) -> String {
    return "public_class_generic_func"
  }
}

public struct PublicStruct {
  public var str = ""
  public init() {}

  public dynamic init(x: Int) { str = "public_struct_init" }

  public dynamic func function() -> String {
    return "public_struct_func"
  }
  public dynamic func genericFunction<T>(_ t: T.Type) -> String {
    return "public_struct_generic_func"
  }
  public dynamic var public_stored_property : String = "public_stored_property"

  public dynamic subscript(_ x: Int) -> String {
    get {
      return "public_subscript_get"
    }
    set {
      str = newValue
    }
  }
  public dynamic subscript(y x: Int) -> String {
    _read {
      yield "public_subscript_get_modify_read"
    }
    _modify {
      yield &str
    }
  }
}

public enum PublicEnumeration<Q> {
  case A
  case B

  public dynamic func function() -> String {
    return "public_enum_func"
  }
  public dynamic func genericFunction<T>(_ t: T.Type) -> String {
    return "public_enum_generic_func"
  }
}

#elseif MODULE2

import Module1

/// Public global functions, struct, class, and enum.

@_dynamicReplacement(for: public_global_func())
public func replacement_for_public_global_func() -> String {
  return "replacement of " + public_global_func()
}

@_dynamicReplacement(for: public_global_generic_func(_:))
public func replacement_for_public_global_generic_func<T>(_ t: T.Type) -> String {
  return "replacement of " + public_global_generic_func(t)
}

extension PublicClass {
  @_dynamicReplacement(for: init(x:))
  convenience public init(y: Int) {
    self.init(x: y)
    str = "replacement of public_class_init"
  }

  @_dynamicReplacement(for: function())
  public func replacement_function() -> String {
    return "replacement of " + function()
  }
  @_dynamicReplacement(for: genericFunction(_:))
  public func replacement_genericFunction<T>(_ t: T.Type) -> String {
    return "replacement of " + genericFunction(t)
  }
}

extension PublicStruct {
  @_dynamicReplacement(for: init(x:))
  public init(y: Int) {
    self.init(x: y)
    str = "replacement of public_struct_init"
  }

  @_dynamicReplacement(for: function())
  public func replacement_function() -> String {
    return "replacement of " + function()
  }
  @_dynamicReplacement(for: genericFunction(_:))
  public func replacement_genericFunction<T>(_ t: T.Type) -> String {
    return "replacement of " + genericFunction(t)
  }
  @_dynamicReplacement(for: public_stored_property)
  var replacement_public_stored_property : String {
    return "replacement of " + public_stored_property
  }
  @_dynamicReplacement(for: subscript(_:))
  subscript(x x: Int) -> String {
    get {
      return "replacement of " + self[x]
    }
    set {
      str = "replacement of " + newValue
    }
  }

  @_dynamicReplacement(for: subscript(y:))
  public subscript(z x: Int) -> String {
    _read {
      yield "replacement of " + self[y: x]
    }
    _modify {
      yield &str
      str = "replacement of " + str
    }
  }
}

extension PublicEnumeration {
  @_dynamicReplacement(for: function())
  public func replacement_function() -> String {
    return "replacement of " + function()
  }
  @_dynamicReplacement(for: genericFunction(_:))
  public func replacement_genericFunction<T>(_ t: T.Type) -> String {
    return "replacement of " + genericFunction(t)
  }
}
#endif
