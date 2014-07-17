//===--- SideCarWriter.h - Side Car Writer ----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the \c SideCarWriter class that writes out source
// side-car data providing additional information about source code as
// a separate input, such as the non-nil/nilable annotations for
// method parameters.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SIDE_CAR_WRITER_H
#define SWIFT_SIDE_CAR_WRITER_H

#include "swift/APINotes/Types.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
namespace side_car {

/// A class that writes side-car data to a binary representation that can be
/// read by the \c SideCarReader.
class SideCarWriter {
  class Implementation;
  Implementation &Impl;

public:
  SideCarWriter();
  ~SideCarWriter();

  SideCarWriter(const SideCarWriter &) = delete;
  SideCarWriter &operator=(const SideCarWriter &) = delete;

  /// Write the side car data to the given stream.
  void writeToStream(llvm::raw_ostream &os);

  /// Add information about a specific Objective-C class.
  ///
  /// \param name The name of this class.
  /// \param info Information about this class.
  void addObjCClass(StringRef name, const ObjCClassInfo &info);

  /// Add information about a specific Objective-C property.
  ///
  /// \param className The class in which this property resides.
  /// \param name The name of this property.
  /// \param info Information about this property.
  void addObjCProperty(StringRef className, StringRef name, 
                       const ObjCPropertyInfo &info);

  /// Add information about a specific Objective-C method.
  ///
  /// \param className The class in which this method resides.
  /// \param selector The selector that names this method.
  /// \param isInstanceMethod Whether this method is an instance method
  /// (vs. a class method).
  /// \param info Information about this method.
  void addObjCMethod(StringRef className, ObjCSelectorRef selector, 
                     bool isInstanceMethod, const ObjCMethodInfo &info);
};

} // end namespace side_car
} // end namespace swift

#endif // LLVM_SWIFT_SIDE_CAR_WRITER_H

