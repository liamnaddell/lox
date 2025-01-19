# Design for compiling and running local variables.

The most important point is that local variables are stored on the stack. We simulate the stack at compile time using the `CTStack` structure, to determine exactly where each local variable will end up.

## Definitions

1. Global variables are variables declared at scope zero
2. Local variables are declared at nonzero scope.

## Variable ID's, Indexes, and names.

Variables are named by `String`'s in lox. I added ID's so that there is a bijection of variable names to variable ID's (`u64`).

If a user names a function or variable `Foo`, the name `Foo` has a unique ID. All variables named `Foo` have the same ID, and all variables with the same numeric ID have the same name.


Global variables have `index`es, which are offsets into `struct VM`'s `globals` vector.

Functions also have `index`es, but these are offsets into the `funcs` vector.

Local variables are the exception, and do NOT! have indexes, these have offsets. Local variables are stored on the stack, and the offset is where on the stack the local variable is stored.

## Description of various structures added for local variable support

### Struct CTStack

When compiling lox code, we need to figure out where on the stack a local variable will end up being stored. Luckily for us, we compile the code in the same order it's evaluated in. As such, we use `struct CTStack`'s decls's array to track how many local variables we saw, which scope they were declared in, and which id's are declared. Then, at runtime, we simply index the offset into the VM's stack.

### next_id and ct_name_to_id

These are used for creating the bijection of variable names and variable ids

### ct_global_id_to_index

Creates a mapping of global variable ID's to global variable indexes.
