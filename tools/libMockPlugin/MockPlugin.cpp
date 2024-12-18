//===--- MockPlugin.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift-c/MockPlugin/MockPlugin.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/JSON.h"

#include <stdio.h>

namespace {
struct TestItem {
  llvm::json::Value expect;
  llvm::json::Value response;
  bool handled = false;

  TestItem(const llvm::json::Value expect, const llvm::json::Value response)
      : expect(expect), response(response) {}
};

struct TestRunner {
  std::vector<TestItem> items;

  TestRunner() {}
  static std::unique_ptr<TestRunner> create(const char *testSpecStr);

  llvm::json::Value createResponse(const TestItem &item,
                                   const llvm::json::Value &req);

  TestItem *findMatchItem(const llvm::json::Value &req);

  int run();
};
} // namespace

std::unique_ptr<TestRunner> TestRunner::create(const char *testSpecStr) {
  // Read test spec.
  auto testSpec = llvm::json::parse(testSpecStr);
  if (!testSpec) {
    llvm::errs() << "failed to parse test spec JSON\n";
    llvm::handleAllErrors(testSpec.takeError(),
                          [](const llvm::ErrorInfoBase &err) {
                            llvm::errs() << err.message() << "\n";
                          });
    return nullptr;
  }

  auto *items = testSpec->getAsArray();
  if (!items) {
    llvm::errs() << "test spec list must be an array\n";
    return nullptr;
  }

  std::unique_ptr<TestRunner> runner(new TestRunner());

  for (auto item : *items) {
    auto *obj = item.getAsObject();
    if (!obj) {
      llvm::errs() << "test spec must be an object\n";
      return nullptr;
    }
    auto *expect = obj->get("expect");
    auto *response = obj->get("response");
    if (!expect || !response) {
      llvm::errs() << "test item must have 'expect' and 'response'\n";
      return nullptr;
    }
    runner->items.emplace_back(*expect, *response);
  }

  return runner;
}

/// Replace string values starting with '=req' (e.g. '"=req.foo.bar[2].baz"') in
/// \p value with the corresponding values from \p req .
static const llvm::json::Value substitute(const llvm::json::Value &value,
                                          const llvm::json::Value &req) {

  // Recursively substitute objects and arrays.
  if (auto *obj = value.getAsObject()) {
    llvm::json::Object copy;
    for (auto &kv : *obj) {
      copy[kv.first] = substitute(kv.second, req);
    }
    return copy;
  }
  if (auto *arr = value.getAsArray()) {
    llvm::json::Array copy;
    for (auto &elem : *arr) {
      copy.push_back(substitute(elem, req));
    }
    return copy;
  }

  auto str = value.getAsString();
  if (!str || !str->starts_with("=req")) {
    // Not a substitution.
    return value;
  }

  auto path = str->substr(4);
  const llvm::json::Value *subst = &req;
  while (!path.empty()) {
    // '.' <alphanum> -> object key.
    if (path.starts_with(".")) {
      if (subst->kind() != llvm::json::Value::Object)
        return "<substitution error: not an object>";
      path = path.substr(1);
      auto keyLength =
          path.find_if([](char c) { return !llvm::isAlnum(c) && c != '_'; });
      auto key = path.slice(0, keyLength);
      subst = subst->getAsObject()->get(key);
      path = path.substr(keyLength);
      continue;
    }
    // '[' <digit>+ ']' -> array index.
    if (path.starts_with("[")) {
      if (subst->kind() != llvm::json::Value::Array)
        return "<substitution error: not an array>";
      path = path.substr(1);
      auto idxlength = path.find_if([](char c) { return !llvm::isDigit(c); });
      size_t idx;
      (void)path.slice(0, idxlength).getAsInteger(10, idx);
      subst = &(*subst->getAsArray())[idx];
      path = path.substr(idxlength);
      if (!path.starts_with("]"))
        return "<substitution error: missing ']' after digits>";
      path = path.substr(1);
      continue;
    }
    // Malformed.
    return "<substitution error: malformed path>";
  }
  return *subst;
}

llvm::json::Value TestRunner::createResponse(const TestItem &item,
                                             const llvm::json::Value &req) {
  auto response = item.response;
  return substitute(response, req);
}

/// Check if \p expect matches \p val .
static bool match(const llvm::json::Value &expect,
                  const llvm::json::Value &val) {
  if (expect.kind() != val.kind())
    return false;

  switch (expect.kind()) {
  case llvm::json::Value::Object: {
    auto *exp = expect.getAsObject();
    auto *obj = val.getAsObject();
    return llvm::all_of(*exp, [obj](const auto &kv) {
      auto *val = obj->get(kv.first.str());
      return val && match(kv.second, *val);
    });
    return true;
  }

  case llvm::json::Value::Array: {
    auto *exp = expect.getAsArray();
    auto *arr = val.getAsArray();
    if (exp->size() != arr->size())
      return false;
    return llvm::all_of_zip(
        *exp, *arr,
        [](const auto &expect, const auto &val) { return match(expect, val); });
  }

  default:
    return expect == val;
  }
}

TestItem *TestRunner::findMatchItem(const llvm::json::Value &req) {
  for (auto &item : items) {
    if (match(item.expect, req)) {
      return &item;
    }
  }
  return nullptr;
}

int TestRunner::run() {
  size_t ioSize;
  while (true) {
    // Read request header.
    uint64_t request_header;
    ioSize = fread(&request_header, sizeof(request_header), 1, stdin);
    if (!ioSize) {
      // STDIN is closed.
      return 0;
    }

    // Read request data.
    auto request_size = llvm::support::endian::byte_swap(
        request_header, llvm::endianness::little);
    llvm::SmallVector<char, 0> request_data;
    request_data.assign(request_size, 0);
    ioSize = fread(request_data.data(), request_size, 1, stdin);
    if (!ioSize) {
      llvm::errs() << "failed to read request data\n";
      return 1;
    }

    // Parse request object.
    auto request_obj =
        llvm::json::parse({request_data.data(), request_data.size()});
    if (!request_obj) {
      llvm::handleAllErrors(
          request_obj.takeError(),
          [](llvm::ErrorInfoBase &err) { llvm::errs() << err.message(); });
      return 1;
    }

    // Handle
    auto *item = findMatchItem(*request_obj);
    if (!item) {
      llvm::errs() << "couldn't find matching item for request: " << *request_obj
                   << "\n";
      return 1;
    }
    if (item->handled) {
      llvm::errs() << "request is already handled\n";
      return 1;
    }
    item->handled = true;
    auto response = createResponse(*item, *request_obj);

    // Write response data.
    llvm::SmallVector<char, 0> response_data;
    llvm::raw_svector_ostream(response_data) << response;
    auto response_size = response_data.size();
    auto response_header =
        llvm::support::endian::byte_swap<uint64_t, llvm::endianness::little>(
            response_size);
    ioSize = fwrite(&response_header, sizeof(response_header), 1, stdout);
    if (!ioSize) {
      llvm::errs() << "failed to write response header\n";
      return 1;
    }
    ioSize = fwrite(response_data.data(), response_size, 1, stdout);
    if (!ioSize) {
      llvm::errs() << "failed to write response data\n";
      return 1;
    }
    fflush(stdout);
  }
}

int _mock_plugin_main(const char *testSpectStr) {
  auto runner = TestRunner::create(testSpectStr);
  if (!runner) {
    return 1;
  }
  return runner->run();
}
