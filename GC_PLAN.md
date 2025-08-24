# Garbage Collection

1. Use std::alloc for malloc and free
2. Make everything repr(C) and use rust Union's for storage
3. Create an ObjPtr type for storing raw ptrs without as much annoyance

# Algorithms

whenever we allocate, we store the ptrs in vm.objects vector.

For marking, we first color to grey and put objs in a work list.

If an object's references are all grey, the object is black and removed from the work list.

Finally remove all the whites (unmarked) from the objects list and free.

After this is implemented, if we want, strings can be implemented as well.
