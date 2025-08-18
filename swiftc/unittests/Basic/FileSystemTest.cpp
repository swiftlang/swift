//===--- FileSystemTest.cpp - Tests for file system utilities -----------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include <gtest/gtest.h>
#include <filesystem>
#include <fstream>
#include <string>

using namespace std;
namespace fs = std::filesystem;

class FileSystemTest : public ::testing::Test {
protected:
  string tempDir;
  
  void SetUp() override {
    tempDir = fs::temp_directory_path() / "swiftc_test";
    fs::create_directories(tempDir);
  }
  
  void TearDown() override {
    if (fs::exists(tempDir)) {
      fs::remove_all(tempDir);
    }
  }
  
  string createTempFile(const string& filename, const string& content) {
    string filepath = tempDir + "/" + filename;
    ofstream file(filepath);
    file << content;
    file.close();
    return filepath;
  }
};

TEST_F(FileSystemTest, FileExists) {
  string filepath = createTempFile("test.swift", "let x = 42");
  
  EXPECT_TRUE(fs::exists(filepath));
  EXPECT_TRUE(fs::is_regular_file(filepath));
  EXPECT_FALSE(fs::is_directory(filepath));
}

TEST_F(FileSystemTest, DirectoryOperations) {
  string dirpath = tempDir + "/subdir";
  fs::create_directory(dirpath);
  
  EXPECT_TRUE(fs::exists(dirpath));
  EXPECT_TRUE(fs::is_directory(dirpath));
  EXPECT_FALSE(fs::is_regular_file(dirpath));
}

TEST_F(FileSystemTest, FileSize) {
  string content = "Hello, World!";
  string filepath = createTempFile("size_test.swift", content);
  
  EXPECT_EQ(fs::file_size(filepath), content.size());
}

TEST_F(FileSystemTest, FileReading) {
  string content = "func main() {\n    print(\"Hello\")\n}";
  string filepath = createTempFile("read_test.swift", content);
  
  ifstream file(filepath);
  string line;
  vector<string> lines;
  
  while (getline(file, line)) {
    lines.push_back(line);
  }
  
  EXPECT_EQ(lines.size(), 3u);
  EXPECT_EQ(lines[0], "func main() {");
  EXPECT_EQ(lines[1], "    print(\"Hello\")");
  EXPECT_EQ(lines[2], "}");
}

TEST_F(FileSystemTest, PathManipulation) {
  fs::path testPath = "/home/user/project/main.swift";
  
  EXPECT_EQ(testPath.filename(), "main.swift");
  EXPECT_EQ(testPath.parent_path(), "/home/user/project");
  EXPECT_EQ(testPath.extension(), ".swift");
  EXPECT_EQ(testPath.stem(), "main");
}

TEST_F(FileSystemTest, RelativePathResolution) {
  fs::path currentDir = fs::current_path();
  fs::path relativePath = "src/main.swift";
  fs::path absolutePath = fs::absolute(relativePath);
  
  EXPECT_TRUE(absolutePath.is_absolute());
  EXPECT_FALSE(relativePath.is_absolute());
}

TEST_F(FileSystemTest, NonExistentFile) {
  string nonExistentPath = tempDir + "/does_not_exist.swift";
  
  EXPECT_FALSE(fs::exists(nonExistentPath));
  EXPECT_THROW(fs::file_size(nonExistentPath), fs::filesystem_error);
}