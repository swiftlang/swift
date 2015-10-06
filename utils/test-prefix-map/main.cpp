#include "swift/Basic/PrefixMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallString.h"
#include <map>
#include <random>
#include <string>

const unsigned RandomSpread = 26000;

using namespace swift;

namespace {
struct Tester {
  std::map<std::string, int> StdMap;
  PrefixMap<char, int> PreMap;

  Tester() {
    assert(PreMap.empty());
  }

  static ArrayRef<char> asArray(StringRef str) {
    return ArrayRef<char>(str.begin(), str.end());
  }
  static StringRef asString(ArrayRef<char> str) {
    return StringRef(str.begin(), str.size());
  }

  void insert(StringRef key, int value) {
    auto stdmapResult = StdMap.insert({key, value});
    auto premapResult = PreMap.insert(asArray(key), value);
    assert(stdmapResult.second == premapResult.second);

    if (premapResult.second) {
      assert(value == *premapResult.first);
    }
  }

  void find(StringRef key) {
    auto stdmapResult = StdMap.lower_bound(key);
    while (stdmapResult == StdMap.end() ||
           !key.startswith(stdmapResult->first)) {
      if (stdmapResult == StdMap.begin()) {
        stdmapResult = StdMap.end();
        break;
      }
      --stdmapResult;
    }
    bool hasStdmapEntry = (stdmapResult != StdMap.end());

    auto premapResult = PreMap.findPrefix(asArray(key));

    assert(hasStdmapEntry == bool(premapResult.first));
    if (!hasStdmapEntry) return;

    assert(key.startswith(stdmapResult->first));
    auto stdmapValue = stdmapResult->second;
    assert(stdmapValue == *premapResult.first);
    assert(premapResult.second == key.begin() + stdmapResult->first.size());
  }

  void clear() {
    StdMap.clear();
    PreMap.clear();
    assert(PreMap.empty());
    assert(PreMap.size() == 0);
  }

  void validate() {
    assert(StdMap.empty() == PreMap.empty());
    assert(StdMap.size() == PreMap.size());

    auto si = StdMap.begin(), se = StdMap.end();
    auto pi = PreMap.begin(), pe = PreMap.end();
    while (true) {
      if (si == se) {
        assert(pi == pe);
        return;
      }

      assert(pi != pe);

      assert(si->second == (*pi).getValue());

      llvm::SmallString<128> buffer;
      assert(StringRef(si->first) == asString((*pi).getKey(buffer)));

      ++si;
      ++pi;
    }
  }

  void dump() {
    llvm::outs() << "StdMap:\n";
    for (auto i = StdMap.begin(), e = StdMap.end(); i != e; ++i) {
      llvm::outs() << "  \"" << i->first << "\": " << i->second << "\n";
    }
    llvm::outs() << "PreMap:\n";
    for (auto i = PreMap.begin(), e = PreMap.end(); i != e; ++i) {
      llvm::SmallVector<char, 128> buffer;
      (*i).getKey(buffer);
      llvm::outs() << "  \"" << buffer << "\": " << (*i).getValue() << "\n";
    }
    PreMap.dump();
  }
};
}

int main(int argc, char **argv) {
  std::random_device randomDevice; // used for seeding
  std::default_random_engine generator(randomDevice());
  std::uniform_int_distribution<unsigned> distribution(0,RandomSpread);

  Tester tester;

  if (argc < 0) tester.dump(); // force this to be used

  auto next = [&] { return distribution(generator); };

  llvm::SmallString<128> key;

  while (true) {
    auto operation = next();

    // Add elements to key.
    if (operation <= .15 * RandomSpread) {
      unsigned n = next() % 1 ? 5 : 7;
      for (unsigned i = 0; i != n; ++i)
        key.push_back('a' + (next() % 26));

    // Remove elements from key.
    } else if (operation <= .3 * RandomSpread) {
      unsigned n = next() % 1 ? 5 : 7;
      for (unsigned i = 0; i != n; ++i)
        if (!key.empty()) key.pop_back();

    // Insert.
    } else if (operation <= .7 * RandomSpread) {
      unsigned value = next();
      llvm::outs() << "  tester.insert(\"" << key << "\", " << value << ");\n";
      tester.insert(key, value);

    // Find.
    } else if (operation <= .98 * RandomSpread) {
      llvm::outs() << "  tester.find(\"" << key << "\");\n";
      tester.find(key);

    // Clear.
    } else {
      llvm::outs() << "  tester.clear();\n";
      tester.clear();
    }

    llvm::outs() << "  tester.validate();\n";
    tester.validate();
  }
}
