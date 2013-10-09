//===--------- GenAxleMeta.h - Axle IR Metadata Generation ----------------===//
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
//  This file implements metadata generation for Axle.
//
//===----------------------------------------------------------------------===//

namespace llvm {
  class Function;
  class Module;
}

namespace swift {
  class SILFunction;
}

namespace axle {
  class GenAxleMeta {
  public:
    GenAxleMeta(llvm::Module& mod);

  private:
    llvm::Module &Module;

  public:
    /// Creates metadata for a function.
    void createFunctionMetadata(swift::SILFunction *SILFn,
                                llvm::Function *IRFn);
  };
}
