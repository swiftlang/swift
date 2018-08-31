// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -o %t/Lib.swiftmodule %s -DLIB
// RUN: %target-build-swift -emit-module -o %t/main.swiftmodule -I %t %s

#if LIB

protocol Proto {}

open class Base<T> {}
public struct ArbitraryStruct {}

extension Base: Proto where T: Proto {}

#else // LIB

import Lib

final class ConcreteSub: Base<ArbitraryStruct> {}

#endif // LIB