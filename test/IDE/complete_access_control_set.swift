// RUN: %batch-code-completion

// ACCESS_CONTROL_SET: Keyword/None:                       set; name=set

public(#^PUBLIC_TOP_LEVEL?check=ACCESS_CONTROL_SET^#) var var1 = 0

private(#^PRIVATE_TOP_LEVEL?check=ACCESS_CONTROL_SET^#) var var2 = 0

internal(#^INTERNAL_TOP_LEVEL?check=ACCESS_CONTROL_SET^#) var var3 = 0

fileprivate(#^FILEPRIVATE_TOP_LEVEL?check=ACCESS_CONTROL_SET^#) var var4 = 0

package(#^PACKAGE_TOP_LEVEL?check=ACCESS_CONTROL_SET^#) var var5 = 0

struct MyStruct {
  public(#^PUBLIC_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop1: Int = 0

  private(#^PRIVATE_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop2: Int = 0

  open(#^OPEN_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop3: Int = 0

  internal(#^INTERNAL_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop4: Int = 0

  fileprivate(#^FILEPRIVATE_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop5: Int = 0

  package(#^PACKAGE_IN_STRUCT?check=ACCESS_CONTROL_SET^#) var prop6: Int = 0
}
