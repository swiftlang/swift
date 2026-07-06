//===--- BodySerializer.h - AST body serialization for cache ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZATION_BODY_SERIALIZER_H
#define SWIFT_SERIALIZATION_BODY_SERIALIZER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {
class DeclContext;

namespace serialization {

class BodySerializer {
  std::string &Output;
  void writeUInt32(uint32_t val);
  void writeUInt8(uint8_t val);
  void writeString(StringRef str);
  void writeType(Type ty);
  void writeDeclRef(const ValueDecl *D);
  void serializeExpr(Expr *E);
  void serializeStmt(Stmt *S);
  void serializeDecl(Decl *D);
  void serializeASTNode(ASTNode node);
public:
  BodySerializer(std::string &output) : Output(output) {}
  void serializeBody(BraceStmt *body);
};

class BodyDeserializer {
  const char *Data;
  size_t Remaining;
  class ASTContext &Ctx;
  class DeclContext *DC;
  uint32_t readUInt32();
  uint8_t readUInt8();
  StringRef readString();
  Type readType();
  ValueDecl *readDeclRef();
  Expr *deserializeExpr();
  Stmt *deserializeStmt();
  Decl *deserializeDecl();
  ASTNode deserializeASTNode();
public:
  BodyDeserializer(StringRef blob, ASTContext &ctx, DeclContext *dc)
      : Data(blob.data()), Remaining(blob.size()), Ctx(ctx), DC(dc) {}
  BraceStmt *deserializeBody();
};

} // namespace serialization
} // namespace swift

#endif
