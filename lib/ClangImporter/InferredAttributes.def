//===--- InferredAttributes.def - Renamed Protocol names --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file describes set of attributes that can be inferred for
// Objective-C classes when they come from a known module.
//

// This fine enumerates the modules, classes, and attributes that will
// be inferred by the Clang importer using the macro:
//
//   INFERRED_ATTRIBUTES(ModuleName, ClassName, AttributeSet)
//
// ModuleName is the name of a module, i.e., CoreData.
// ClassName is the name of the class, i.e., NSManagedObject
// AttributeSet is an OR of attribute names, i.e., requires_stored_property_inits
//
//===----------------------------------------------------------------------===//

#ifndef INFERRED_ATTRIBUTES
#  define INFERRED_ATTRIBUTES(ModuleName, ClassName, AttributeSet)
#endif

INFERRED_ATTRIBUTES(CoreData, NSManagedObject, requires_stored_property_inits)

#undef INFERRED_ATTRIBUTES
