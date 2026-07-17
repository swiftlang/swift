#ifndef SWIFT_CXXMETHODBRIDGING_H
#define SWIFT_CXXMETHODBRIDGING_H

#include "clang/AST/Attr.h"
#include "clang/AST/DeclCXX.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {

struct CXXMethodBridging {
  enum class Kind { unknown, getter, setter, subscript };

  enum class NameKind { unknown, snake, lower, camel, title };

  explicit CXXMethodBridging(const clang::CXXMethodDecl *method)
      : method(method) {}

  Kind classify() {
    if (nameIsBlacklist())
      return Kind::unknown;

    auto nameKind = classifyNameKind();
    if (nameKind != NameKind::title && nameKind != NameKind::camel &&
        nameKind != NameKind::lower && nameKind != NameKind::snake)
      return Kind::unknown;

    if (hasSetterPrefix()) {
      // Setters only have one parameter.
      if (method->getNumParams() != 1)
        return Kind::unknown;

      // rdar://89453106 (We need to handle imported properties that return a
      // reference)
      if (method->getParamDecl(0)->getType()->isReferenceType())
        return Kind::unknown;

      return Kind::setter;
    }

    // Getters and subscripts cannot return void.
    if (method->getReturnType()->isVoidType())
      return Kind::unknown;

    // Getters cannot take arguments.
    if (method->getNumParams() != 0)
      return Kind::unknown;

    // rdar://89453106 (We need to handle imported properties that return a
    // reference)
    if (method->getReturnType()->isReferenceType())
      return Kind::unknown;

    // A getter is named with a "get" prefix, or -- when explicitly annotated
    // with SWIFT_COMPUTED_PROPERTY -- with any name.
    if (hasGetterPrefix() || isExplicitComputedProperty())
      return Kind::getter;

    // rdar://89453187 (Add subscripts clarification to CXXMethod Bridging to
    // clean up importDecl)
    return Kind::unknown;
  }

  NameKind classifyNameKind() {
    bool hasUpper = llvm::any_of(
        getClangName(), [](unsigned char ch) { return std::isupper(ch); });

    if (getClangName().empty())
      return NameKind::unknown;

    // Any name containing an underscore is treated as snake_case, even when it
    // also has uppercase letters (e.g. an acronym segment like
    // `get_http_URL`).
    if (getClangName().contains('_'))
      return NameKind::snake;
    if (!hasUpper)
      return NameKind::lower;

    return islower(getClangName().front()) ? NameKind::camel : NameKind::title;
  }

  llvm::StringRef getClangName() {
    if (!method->getDeclName().isIdentifier())
      return "";

    return method->getName();
  }

  bool hasGetterPrefix() { return getClangName().starts_with_insensitive("get"); }
  bool hasSetterPrefix() { return getClangName().starts_with_insensitive("set"); }

  bool isExplicitComputedProperty() {
    for (const auto *attr : method->specific_attrs<clang::SwiftAttrAttr>())
      if (attr->getAttribute() == "import_computed_property")
        return true;
    return false;
  }

  llvm::StringRef nameWithoutAccessorPrefix() {
    if (hasGetterPrefix() || hasSetterPrefix())
      return getClangName().drop_front(3);
    return getClangName();
  }

  std::string importNameAsCamelCaseName() {
    std::string output;
    auto kind = classify();
    if (kind == Kind::getter || kind == Kind::setter) {
      output = nameWithoutAccessorPrefix().str();
    } else {
      output = getClangName().str();
    }

    if (output.empty())
      return output;

    // No work to do.
    if (classifyNameKind() == NameKind::lower)
      return output;

    // The first character is always lowercase.
    output.front() = std::tolower(output.front());

    if (classifyNameKind() == NameKind::snake) {
      for (std::size_t i = 0; i < output.size(); i++) {
        size_t next = i + 1;
        if (output[i] == '_') {
          // If the first or last element is an underscore, remove it.
          if (i == 0 || next == output.size()) {
            output.erase(i, 1);
          } else if (next < output.size()) {
            // If the current element is an underscore, capitalize the element
            // next to it, and remove the extra element.
            output[i] = std::toupper(output[next]);
            output.erase(next, 1);
          }
        }
      }
      // A property name is always lowercased first. When the name started with
      // a leading underscore (e.g. `Get_X` -> `_X` -> `X`), the real first
      // letter wasn't lowercased above, so do it now.
      if (!output.empty())
        output.front() = std::tolower(output.front());
      return output;
    }

    // We already lowercased the first element, so start at one. Look at the
    // current element and the next one. To handle cases like UTF8String, start
    // making all the uppercase characters lower, until we see an upper case
    // character followed by a lower case character (i.e., "St").
    for (size_t i = 1; i < output.size(); i++) {
      size_t next = i + 1;

      // If we see two upper case characters (or an upper case character and a
      // number) make the current character lower case.
      if (std::isupper(output[i]) &&
          (std::isupper(output[next]) || std::isdigit(output[next]))) {
        output[i] = std::tolower(output[i]);
        // If we found an upper case character followed by a lower case
        // character, we went far enough. We're done.
      } else if (std::isupper(output[i]) && std::islower(output[next])) {
        break;
        // If we got to the end of the string, we're done.
      } else if (std::isupper(output[i]) && next + 1 > output.size()) {
        output[i] = std::tolower(output[i]);
        break;
      }
    }

    return output;
  }

  std::string importNameAsTitleCaseName() {
    auto output = importNameAsCamelCaseName();
    output.front() = std::toupper(output.front());
    return output;
  }

private:
  const clang::CXXMethodDecl *method = nullptr;

  bool nameIsBlacklist() {
    auto loweredName = getClangName().lower();
    // Names that start with "get" or "set" but aren't getters or setters.
    return loweredName == "getter" || loweredName == "setter" ||
           loweredName == "get" || loweredName == "set";
  }
};

} // namespace swift

#endif // SWIFT_CXXMETHODBRIDGING_H
