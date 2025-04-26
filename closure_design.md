# How were closures implemented

## Name resolution

Name resolution (vpass) involves correlating identifiers to users, i.e. which `fun` block defines the function `a` in the current scope. 

For upvalues, Name Resolution was extended:
* to mark when a variable access is an upvalue access
* To create a list of upvalues used in a function (or its descendents). Indexes into which are called slots.
* To correlate upvalues to slots

## Compilation

Compilation (compile.rs) was modified to pass the information produced in vpass to BC.
I.e. emitting the correct getters and setters, and passing the list of utilized upvalues per function
to BC.

## Bytecode

The majority of upvalue work went into BC. Three opcodes were added:

* OP_CLOSURE: This creates a closure on the stack, and also closes any upvalues required by the closure.
* OP_GET/SET_UPVALUE: This takes a "slot" parameter and get/set's the upvalue.

We also added closures as values, meaning closures can be assigned to variables.

### What is an upvalue in BC?

Upvalues exist in two states, and can be of two kinds, for a matrix of 3 possible "upvalues". 

#### Upvalue States

An upvalue can be closed or open. Upvalues start as open, but become closed when "closed" over.
I.e. when OP_CLOSURE is called, and we need to copy the upvalue to the heap instead of the 
stack to prevent it being deallocated.

#### Upvalue Kinds

An upvalue can either be local or non-local. A local upvalue is defined in our parent function.
non-local upvalues might be defined in our grandparent (or beyond!) function.

#### Example

Here's an example for all these upvalue kinds
```
fun a() {
    # Local variable
    aa = 0;
    # Fun b has aa in it's upvalues array despite never utilizing aa because fun c uses aa.
    fun b() {
        # Local variable
        bb = 0;
        func c() {
            # Local variable;
            cc = 0;
            # Getting a local upvalue
            print bb;
            # Getting a non-local upvalue;
            print cc;
        }
        # Technically this creates a closure (closing the upvalue) then calls the closure.
        c();
        # Creating a closure and returning it.
        return c;
    }
    b();
    return b;
}
var b = a();
var c = b();
c();
```
