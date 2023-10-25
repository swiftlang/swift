//===--- ClangClassTemplateNamePrinter.h ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CLANG_TEMPLATE_NAME_PRINTER_H
#define SWIFT_CLANG_TEMPLATE_NAME_PRINTER_H

#include "ImportName.h"
#include "swift/AST/ASTContext.h"
#include "clang/AST/DeclTemplate.h"

namespace swift {
namespace importer {

/// Returns a Swift representation of a C++ class template specialization name,
/// e.g. "vector<CWideChar, allocator<CWideChar>>".
///
/// This expands the entire tree of template instantiation names recursively.
/// While printing deep instantiation levels might not increase readability, it
/// is important to do because the C++ templated class names get mangled,
/// therefore they must be unique for different instantiations.
///
/// This function does not instantiate any templates and does not modify the AST
/// in any way.
std::string printClassTemplateSpecializationName(
    const clang::ClassTemplateSpecializationDecl *decl, ASTContext &swiftCtx,
    NameImporter *nameImporter, ImportNameVersion version);

} // namespace importer
} // namespace swift

#endif // SWIFT_CLANG_TEMPLATE_NAME_PRINTER_H
