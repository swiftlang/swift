import B
import C

public func use() {
  fromB()
  #if USEC
  fromC()
  #endif
}
