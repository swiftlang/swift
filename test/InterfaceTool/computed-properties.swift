// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
public struct Props {
  public var stored: Int = 42

  public var computed: Int {
    return stored * 2
  }

  public var getSet: Int {
    get { return stored }
    set { stored = newValue }
  }

  @inlinable public var inlinableComputed: Int {
    return stored + 1
  }
}
//--- expected.swift
public struct Props {
  public var stored: Int = 42

  public var computed: Int {
      get
  }

  public var getSet: Int {
    get
    set
  }

  @inlinable public var inlinableComputed: Int {
    return stored + 1
  }
}
