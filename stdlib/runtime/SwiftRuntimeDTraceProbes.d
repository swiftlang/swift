
provider swift {
  probe retain();
  probe release();
  probe allocateObject();
  probe deallocateObject();
};
