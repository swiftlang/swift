for test in test/Interpreter/SDK/*; do
  echo `basename $test`":"
  time swift -parse "$test"
  echo
done
