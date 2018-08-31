//===--- FixitFilter.h - Migrator Fix-It Filter -----------------*- C++ -*-===//
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
//
// This class filters fix-its that are interesting to the Migrator.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_FIXITFILTER_H
#define SWIFT_MIGRATOR_FIXITFILTER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticsSema.h"

namespace swift {
namespace migrator {

struct FixitFilter {
  /// Returns true if the fix-it should be applied.
  bool shouldTakeFixit(const DiagnosticKind Kind,
                       const DiagnosticInfo &Info) const {
    // Do not add a semi or comma as it is wrong in most cases during migration
    if (Info.ID == diag::statement_same_line_without_semi.ID ||
        Info.ID == diag::declaration_same_line_without_semi.ID ||
        Info.ID == diag::expected_separator.ID)
      return false;
    // The following interact badly with the swift migrator, they are undoing
    // migration of arguments to preserve the no-label for first argument.
    if (Info.ID == diag::witness_argument_name_mismatch.ID ||
        Info.ID == diag::missing_argument_labels.ID ||
        Info.ID == diag::override_argument_name_mismatch.ID)
      return false;
    // This also interacts badly with the swift migrator, it unnecessary adds
    // @objc(selector) attributes triggered by the mismatched label changes.
    if (Info.ID == diag::objc_witness_selector_mismatch.ID ||
        Info.ID == diag::witness_non_objc.ID)
      return false;
    // This interacts badly with the migrator. For such code:
    //   func test(p: Int, _: String) {}
    //   test(0, "")
    // the compiler bizarrely suggests to change order of arguments in the call
    // site.
    if (Info.ID == diag::argument_out_of_order_unnamed_unnamed.ID)
      return false;
    // The following interact badly with the swift migrator by removing @IB*
    // attributes when there is some unrelated type issue.
    if (Info.ID == diag::invalid_iboutlet.ID ||
        Info.ID == diag::iboutlet_nonobjc_class.ID ||
        Info.ID == diag::iboutlet_nonobjc_protocol.ID ||
        Info.ID == diag::iboutlet_nonobject_type.ID ||
        Info.ID == diag::iboutlet_only_mutable.ID ||
        Info.ID == diag::invalid_ibdesignable_extension.ID ||
        Info.ID == diag::invalid_ibinspectable.ID ||
        Info.ID == diag::invalid_ibaction_decl.ID)
      return false;

    // The Migrator only applies changes from the APIDiffMigratorPass in the
    // primary file, so if a nominal type was renamed, for example, any members
    // users have added in an extension in a separate file may not be visible,
    // due to the extension rename not having been applied. The below diag(s)
    // can provide undesireable fixits that rename references of these
    // no-longer-visible members to similar still-visible ones.
    // Note: missing_argument_lables and extra_argument_labels are filtered out
    // elsewhere
    if (Info.ID == diag::wrong_argument_labels.ID)
      return false;

    // Adding type(of:) interacts poorly with the swift migrator by
    // invalidating some inits with type errors.
    if (Info.ID == diag::init_not_instance_member.ID)
      return false;
    // Renaming enum cases interacts poorly with the swift migrator by
    // reverting changes made by the migrator.
    if (Info.ID == diag::could_not_find_enum_case.ID)
      return false;

    // Sema suggests adding both `@objc` and `@nonobjc` as alternative fix-its
    // for inferring Swift-3 style @objc visibility, but we don't want the
    // migrator to suggest `@nonobjc`.
    if (Info.ID == diag::objc_inference_swift3_addnonobjc.ID) {
      return false;
    }

    // With SE-110, the migrator may get a recommendation to add a Void
    // placeholder in the call to f in:
    // func foo(f: (Void) -> ()) {
    //   f()
    // }
    // Here, f was () -> () in Swift 3 but now (()) -> () in Swift 4. Adding a
    // type placeholder in the f() call isn't helpful for migration, although
    // this particular fix-it should be to fix the f parameter's type.
    if (Info.ID == diag::missing_argument_named.ID ||
        Info.ID == diag::missing_argument_positional.ID) {
      return false;
    }

    // Trying to add '_ in' to a closure signature can be counterproductive when
    // fixing function signatures like (Void) -> () to () -> ().
    if (Info.ID == diag::closure_argument_list_missing.ID) {
      return false;
    }

    // The type-checker can erroneously report this diagnostic in the case of
    // mismatching closure arguments to things that now take a tuple via SE-0110.
    if (Info.ID == diag::extra_argument_labels.ID) {
      return false;
    }

    if (Kind == DiagnosticKind::Error)
      return true;

    // Fixits from warnings/notes that should be applied.
    if (Info.ID == diag::forced_downcast_coercion.ID ||
        Info.ID == diag::forced_downcast_noop.ID ||
        Info.ID == diag::function_type_no_parens.ID ||
        Info.ID == diag::convert_let_to_var.ID ||
        Info.ID == diag::parameter_extraneous_double_up.ID ||
        Info.ID == diag::noescape_parameter.ID ||
        Info.ID == diag::where_inside_brackets.ID ||
        Info.ID == diag::selector_construction_suggest.ID ||
        Info.ID == diag::selector_literal_deprecated_suggest.ID ||
        Info.ID == diag::attr_noescape_deprecated.ID ||
        Info.ID == diag::attr_autoclosure_escaping_deprecated.ID ||
        Info.ID == diag::attr_warn_unused_result_removed.ID ||
        Info.ID == diag::any_as_anyobject_fixit.ID ||
        Info.ID == diag::deprecated_protocol_composition.ID ||
        Info.ID == diag::deprecated_protocol_composition_single.ID ||
        Info.ID == diag::deprecated_any_composition.ID ||
        Info.ID == diag::deprecated_operator_body.ID ||
        Info.ID == diag::unbound_generic_parameter_explicit_fix.ID ||
        Info.ID == diag::objc_inference_swift3_addobjc.ID ||
        Info.ID == diag::objc_inference_swift3_dynamic.ID ||
        Info.ID == diag::override_swift3_objc_inference.ID ||
        Info.ID == diag::objc_inference_swift3_objc_derived.ID ||
        Info.ID == diag::missing_several_cases.ID ||
        Info.ID == diag::missing_particular_case.ID ||
        Info.ID == diag::missing_unknown_case.ID ||
        Info.ID == diag::paren_void_probably_void.ID ||
        Info.ID == diag::make_decl_objc.ID ||
        Info.ID == diag::optional_req_nonobjc_near_match_add_objc.ID)
      return true;

    return false;
  }
};
} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_FIXITFILTER_H
