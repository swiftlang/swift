//===--- TensorFlow.cpp - TensorFlow lowering support ---------------------===//
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

#include "TensorFlow.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "llvm/ADT/StringExtras.h"
#include "swift/SIL/SILModule.h"
using namespace swift;
using namespace tf;

template<typename...T, typename...U>
static InFlightDiagnostic
diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}


/// Return true if the specified source location is inside of the tensor
/// library, which should be opaque to the user.
///
/// FIXME: This is a hack: eventually, these routines will be in their own
/// module, and we'll just recognize them by what module they are in, just like
/// how the swift stdlib is a well-known module for the compiler.
static bool isTensorLibraryInternal(SILLocation loc, ASTContext &Ctx) {
  if (loc.getSourceLoc().isInvalid())
    return false;
  auto str = Ctx.SourceMgr.getBufferIdentifierForLoc(loc.getSourceLoc());
  if (str.contains("Sources/TensorOps.swift") ||
      str.contains("Sources/Tensor.swift") ||
      str.contains("Sources/RankedTensor.swift") ||
      str.contains("Sources/TensorXD.swift") ||
      str.contains("Sources/PythonGlue.swift"))
    return true;

  return false;
}

/// If the specified type is the well-known TensorHandle<T> type, then return
/// "T".  If not, return a null type.
Type tf::isTensorHandle(Type ty) {
  if (auto bgct = ty->getAs<BoundGenericClassType>()) {
    if (bgct->getDecl()->getNameStr() == "TensorHandle") {
      assert(bgct->getGenericArgs().size() == 1 && "Expected one generic arg");
      return bgct->getGenericArgs()[0];
    }
  }
  return Type();
}


/// Given a function name that might refer to a tensorflow op function, this
/// returns the op name and operand description and returns true.  If the
/// function name doesn't correspond to an op, this returns false.
static bool decodeTensorOpName(StringRef name, StringRef &opName,
                               StringRef &typeInfo) {
  // Op functions are expected to be of the form:
  //  ...__tfop_<OPNAME>__<OPERANDDESC>__...
  // an example for the 'Add' op might be _T013__tfop_Add__Tf4gg_n
  auto pos = name.find("__tfop_");
  if (pos == StringRef::npos) return false;
  name = name.substr(pos+strlen("__tfop_"));

  pos = name.find("__");
  if (pos == StringRef::npos) return false;
  opName = name.substr(0, pos);
  name = name.substr(pos+strlen("__"));

  pos = name.find("__");
  if (pos == StringRef::npos) return false;
  typeInfo = name.substr(0, pos);
  return true;
}

/// This decodes the type information mangled into the SIL name for a
/// function.  This takes the operand description itself, not the fn name.  It
/// returns true on failure.
static bool decodeTensorOperandInfo(StringRef name,
                                    SmallVectorImpl<OpCommand> &result) {
  result.clear();
  for (auto c : name) {
    switch (c) {
    case 't': result.push_back(OpCommand::Tensor); break;
    case 'c': result.push_back(OpCommand::Constant); break;
    case 'd': result.push_back(OpCommand::AddDType); break;
    default: return true;
    }
  }
  return false;
}

/// If the specified value is a valid value for a constant operand, return the
/// literal it is initialized to, otherwise null.
LiteralInst *TensorOpInfo::getTensorConstantOperand(SILValue v) {
  // If this is an integer or fp literal, we succeed.
  if (isa<IntegerLiteralInst>(v) || isa<FloatLiteralInst>(v))
    return cast<LiteralInst>(v);

  // Handle the form where a StructInst is wrapping a literal (e.g. Int/Float).
  if (auto *SI = dyn_cast<StructInst>(v)) {
    auto SO = SI->getNumOperands() == 1 ? SI->getOperand(0) : 0;
    return SO ? getTensorConstantOperand(SO) : nullptr;
  }

  // Because we're often coming from generic code, we frequently get a constant
  // passed by-address.  Check for an alloc_stack with a single store to it and
  // consume the stored value.
  if (auto *ASI = dyn_cast<AllocStackInst>(v)) {
    if (auto *store = ASI->getSingleUserOfType<StoreInst>())
      return getTensorConstantOperand(store->getSrc());
  }

  return nullptr;
}


/// Return true and fill in this struct if this is a tensor operation that
/// should be partitioned out to run on the accelerator.
bool TensorOpInfo::decode() {
  // Tuple extracts of tensor ops are considered to be themselves Tensor
  // operations, since they are part of the core representation of nodes that
  // produce multiple results.
  if (auto *ti = dyn_cast<TupleExtractInst>(inst))
    if (auto *ai = dyn_cast<BuiltinInst>(ti->getOperand())) {
      inst = ai;
      return decode();
    }

  StringRef mangledName;

  // Tensor operations are builtin instructions.
  if (auto *bi = dyn_cast<BuiltinInst>(inst)) {
    mangledName = bi->getName().str();
  } else {
    return false;
  }

  // If the name is valid, it isn't an op.
  StringRef typeDescriptorStr;
  if (!decodeTensorOpName(mangledName, opName, typeDescriptorStr))
    return false;

  auto diagInvalid = [&](std::string problem) {
    diagnose(inst->getModule().getASTContext(), inst->getLoc().getSourceLoc(),
             diag::tfop_incorrect_silname_operandinfo,
             operandDescriptorStr, problem);
  };

  // The type descriptor has operand and result info separated by a colon.
  auto colonLoc = typeDescriptorStr.find(':');
  if (colonLoc == StringRef::npos) {
    diagInvalid("no colon in type descriptor");
    return false;
  }

  operandDescriptorStr = typeDescriptorStr.take_front(colonLoc);
  resultDescriptorStr = typeDescriptorStr.drop_front(colonLoc+1);

  if (operandDescriptorStr.empty()) {
    diagInvalid("no type descriptor string found");
    return false;
  }

  if (decodeTensorOperandInfo(operandDescriptorStr, operandDescriptors)) {
    diagInvalid("unknown letter");
    return false;
  }


  // Validate that this instruction is ok.
  unsigned nextOperand = 0;
  auto getNextOperand = [&]() -> SILValue {
    // If we ran out of operands, something is wrong.
    if (nextOperand >= inst->getNumOperands()) {
      diagInvalid("expected more operands than the " +
                  llvm::utostr(inst->getNumOperands()-1) + " present");
      return SILValue();
    }
    return inst->getOperand(nextOperand++);
  };

  for (auto opInfo : operandDescriptors) {
    switch (opInfo) {
      case OpCommand::Tensor: {
        auto op = getNextOperand();
        if (!op || !isTensorHandle(op->getType().getSwiftRValueType())) {
          diagInvalid("expected " +
                      llvm::utostr(nextOperand-2) + " to be a tensor");
          return false;
        }
        break;
      }
      case OpCommand::AddDType: {
        auto op = getNextOperand();
        if (!op || !isa<MetatypeInst>(op)) {
          diagInvalid("metatype expected for 'd' operand");
          return false;
        }
        break;
      }
      case OpCommand::Constant: {
        // If this requires a constant value and doesn't have one (i.e., it's a
        // variable), then we handle this as valid, but as a non-TensorOp.  The
        // value will be computed on the host and be sent over.
        auto op = getNextOperand();
        if (!op) return false;

        // If it isn't a literal, don't treat it like a tensor op.
        if (!getTensorConstantOperand(op))
          return false;
        break;
      }
    }
  }

  // Diagnose when the type descriptor didn't specify enough args.
  if (nextOperand != inst->getNumOperands()) {
    diagInvalid("more arguments present than type descriptors specified");
    return false;
  }

  return true;
}


/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILLocation tf::getUserSourceLocation(SILLocation loc, SILNode *value) {

  // If we are dealing with a SILInstruction, we can produce the location
  // that the instruction was inlined into, which is a better place to report
  // as the location of the diagnostic.
  auto *inst = dyn_cast<SILInstruction>(value);
  if (!inst) return loc;

  return getUserSourceLocation(inst->getDebugLocation()).getLocation();
}

/// The SIL location for operations we process are usually deep in the bowels
/// of the tensor library code, which are all implementation details to the
/// user.  As such, walk the inlining location of the specified node to return
/// the first location *outside* of the tensor implementation goop.
SILDebugLocation tf::getUserSourceLocation(SILDebugLocation loc) {
  auto ds = loc.getScope();

  // Zip through inlined call site information that came from the
  // implementation guts of the tensor library.  We want to report the
  // message inside the user's code, not in the guts we inlined through.
  for (; auto ics = ds->InlinedCallSite; ds = ics) {
    // If we reach the user code, or if the inlined call site is invalid,
    // stop scanning.
    if (ics->Loc.isNull())
      break;

    if (SILFunction *F = ds->getInlinedFunction()) {
      if (!isTensorLibraryInternal(F->getLocation(), F->getASTContext()))
        break;
    }
  }

  if (!ds->Loc.isNull())
    return SILDebugLocation(ds->Loc, ds);

  return loc;
}

/// Return true if the specified type is a valid tensor element type.  For
/// example, int128 and pointers are not.
///
/// TODO: This should eventually consider information about the target
/// deployment.
///
bool tf::isValidTensorFlowElementType(Type ty) {
  if (auto *BIF = ty->getAs<BuiltinFloatType>()) {
    switch (BIF->getFPKind()) {
    case BuiltinFloatType::IEEE16:
    case BuiltinFloatType::IEEE32:
    case BuiltinFloatType::IEEE64: return true;
    case BuiltinFloatType::IEEE80:
    case BuiltinFloatType::IEEE128:
    case BuiltinFloatType::PPC128:
      return false;
    }
  }

  if (auto *BII = ty->getAs<BuiltinIntegerType>()) {
    if (BII->getWidth().isPointerWidth())
      return true;

    switch (BII->getFixedWidth()) {
    case 1:
    case 8:
    case 16:
    case 32:
    case 64:
      return true;
    default: return false;
    }
  }
  return false;
}
